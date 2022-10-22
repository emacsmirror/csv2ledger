;;; csv2ledger.el --- Convert csv files to ledger entries  -*- lexical-binding: t -*-

;; Copyright (c) 2022 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2022
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (parse-csv) (dash "2.19.1"))

;; This file is NOT part of GNU Emacs.

;; csv2ledger is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; csv2ledger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are functions for personal use with ledger-cli and ledger-mode.

;;; Code:

(require 'subr-x)
(require 'parse-csv)
(require 'csv-mode)
(require 'dash)

(defgroup csv2ledger nil
  "Csv2Ledger: Converting csv files to ledger entries."
  :group 'ledger
  :group 'CSV)

(defcustom c2l-accounts-file nil
  "File containing account declarations.
This should point to a ledger file that defines the accounts.  It
can be a separate file or a ledger file containing transactions."
  :type 'file
  :group 'csv2ledger)

(defcustom c2l-base-account nil
  "Base ledger account.
A CSV file normally lists transactions for a single bank account.
The base ledger account is the ledger account associated with
this bank account.  As such, it is the account that will turn up
in every transaction read from the CSV file.

This is a buffer-local variable and can be set in the local
variable block of a CSV file.  Alternatively, one can set a
default value if one only has one back account from which CSV
files are processed."
  :type 'file
  :group 'csv2ledger
  :local t)

(defcustom c2l-fallback-balancing-account nil
  "Fallback for the balancing account in transactions.
When creating a ledger entry, csv2ledger tries to determine the
balancing account for the transaction based on the matchers in
`c2l-account-matchers-file'.  If no acccount is found, the value
of this variable is used.  If the value is unset, the user is
asked for an account."
  :type 'string
  :group 'csv2ledger)

(defcustom c2l-account-holder nil
  "Regular expression matching the account holder.
If the payee matches this regular expression, the sender is used
instead of the payee as the title of the entry."
  :type 'string
  :group 'csv2ledger)

(defcustom c2l-csv-columns '(date valuation description sender payee amount)
  "List of columns in the CSV file.
The data in the CSV file is extracted based on this list.  The
order of elements in the list should therefore represent the
order of columns in the CSV file.  A column that is not relevant
can be labeled with an underscore."
  :type '(repeat symbol)
  :group 'csv2ledger)

(defcustom c2l-field-parse-functions '((date        . identity)
                                       (valuation   . identity)
                                       (description . identity)
                                       (sender      . identity)
                                       (payee       . identity)
                                       (amount      . identity))
  "List of functions to modify fields in an entry.
These functions should take a single string argument and should
return a string."
  :type '(repeat (cons (symbol :tag "Field") function))
  :group 'csv2ledger)

(defcustom c2l-title-function #'c2l-payee-or-sender
  "Function to create a title.
The function should take as argument an entry alist of
field-value pairs and should return a string.  The string
returned is used as the title of the ledger entry,"
  :type 'function
  :group 'csv2ledger)

(defcustom c2l-account-matchers-file nil
  "File containing matcher strings mapped to accounts.
This should be a TSV (tab-separated values) file containing one
matcher per line:

aldi          Expenses:Groceries
lidl          Expenses:Groceries
restaurant    Expenses:Leisure:Restaurant

where the two columns are separated by a TAB.

The matcher is a string (not a regular expression).  If a matcher
is found in any of the fields listed in `c2l-title-match-fields',
the corresponding account is used to book the transaction."
  :type 'file
  :group 'csv2ledger)

(defcustom c2l-balancing-match-fields '(payee description)
  "List of fields used for determining the balancing account.
Fields in this list are matched against the matchers in
`c2l-account-matchers-file'.  Note that the order of the fields
in this list can be relevant, because the first field that
returns a match wins."
  :type '(repeat symbol)
  :group 'csv2ledger)

(defcustom c2l-auto-cleared nil
  "If non-nil, mark every entry as cleared.
This puts an asterisk between the date and the payee."
  :type 'boolean
  :group 'csv2ledger)

(defcustom c2l-alignment-column 52
  "The column to which amounts are aligned.
This should most likely be set to the same value as
`ledger-post-amount-alignment-column'."
  :type 'integer
  :group 'csv2ledger)

(defvar c2l--accounts nil "List of ledger accounts, mainly used for completion.")
(defvar c2l--compiled-matcher-regexes nil "Alist of accounts and their matchers.")
(defvar c2l--results-buffer nil "Buffer for conversion results.")

(defun c2l-convert-little-endian-to-iso8601-date (date)
  "Convert DATE from a little-endian format to an ISO 8601 format.
DATE should be a string representing a date of the form
DD.MM.YYYY.  Return value is a date string of the form YYYY-MM-DD.

Note that the input date may have dots, dashes or forward slashes
separating the date parts; also, additional whitespace is
removed.  This function does not check if DATE has a valid date
format, it just splits DATE on the separator, reverses the date
parts and joins them again."
  (string-join (nreverse (split-string date "[./-]" t "[[:space:]]")) "-"))

(defun c2l-payee-or-sender (entry)
  "Return payee or sender based on `c2l-account-holder'.
This function is for use as the value of `c2l-title-function'.
ENTRY should be an alist containing field-value pairs for an
entry and should contain values for `payee' and `sender'.  If the
value of `c2l-account-holder' matches the payee, the sender is
returned, otherwise the payee is returned."
  (when (stringp c2l-account-holder)
    (let ((payee (alist-get 'payee entry))
         (sender (alist-get 'sender entry)))
     (if (string-match-p c2l-account-holder payee)
         sender
       payee))))

(defun c2l--compose-entry (items &optional from to)
  "Create a ledger entry.
ITEMS is an alist containing (key . value) pairs that should be
included in the entry.  It should contain values for the keys
`date', `payee', `sender' and `amount'.  ITEMS may also contain a
value for `description'.  If it is present, it is added as a
comment, preceded by \"Desc:\".

FROM is the account where the money comes from, TO the account to
which it goes.  Note that if AMOUNT is negative, these roles are
reversed.  FROM and TO default to `c2l-fallback-balancing-account' and
`c2l-base-account', respectively."
  (or from (setq from c2l-fallback-balancing-account))
  (or to (setq to c2l-base-account))
  (let* ((parsed-items (mapcar (lambda (item)
                                 (let ((field (car item))
                                       (value (cdr item)))
                                   (cons field
                                         (funcall (alist-get field c2l-field-parse-functions #'identity) value))))
                               items))
         (title (funcall c2l-title-function parsed-items)))
    (let-alist parsed-items
      (concat .date (if .valuation (format "=%s " .valuation) "") (if c2l-auto-cleared " *" "") " " title "\n"
              (if (and .description (not (string-empty-p .description))) (format "    ; Desc: %s\n" .description) "")
              (format "    %s\n" from)
              (format "    %s  " to)
              (make-string (- c2l-alignment-column 4 (length to) 2 (length .amount)) ?\s)
              .amount "\n"))))

(defun c2l--read-accounts (file)
  "Read list of accounts from FILE."
  (when (stringp file)
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let (accounts)
            (while (not (eobp))
              (if (looking-at "^account \\([[:print:]]+\\)$")
                  (push (match-string 1) accounts))
              (forward-line 1))
            accounts))
      (user-error "[Csv2Ledger] Accounts file `%s' not found" file))))

(defun c2l--read-account-matchers (file)
  "Read account matchers from FILE.
See the documentation for the variable
`c2l-account-matchers-file' for details on the matcher file."
  (when (stringp file)
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let (accounts)
            (while (looking-at "\\([[:print:]]+\\)\t\\([[:print:]]+\\)")
              (let ((matcher (match-string 1))
                    (account (match-string 2)))
                (push (cons matcher account) accounts))
              (forward-line 1))
            accounts))
      (user-error "[Csv2Ledger] Account matcher file `%s' not found" file))))

(defun c2l--compile-matcher-regexes (accounts)
  "Create efficient regular expressions for the matchers in ACCOUNTS.
ACCOUNTS is a list of (<matcher> . <account>) conses, where
<matcher> should be unique but <account> may occur multiple
times.  Return value is an alist in which each account in
ACCOUNTS is mapped to a regular expression matching all matchers
for that account."
  (mapcar (lambda (e)
            (cons (car e)
                  (regexp-opt (mapcar #'car (cdr e)))))
          (seq-group-by #'cdr accounts)))

(defun c2l--match-account (str)
  "Try to match STR to an account."
  (unless c2l--compiled-matcher-regexes
    (setq c2l--compiled-matcher-regexes
          (-> c2l-account-matchers-file
              (c2l--read-account-matchers)
              (c2l--compile-matcher-regexes))))
  (--some (if (string-match-p (cdr it) str)
              (car it))
          c2l--compiled-matcher-regexes))

(defun c2l--csv-line-to-ledger (row)
  "Convert ROW to a ledger entry.
ROW contains the data of the entry as a list of strings.  The
strings are interpreted according to the template in
`c2l-csv-columns'.  The transaction is booked to the account in
`c2l-base-account'.  The reverse account is determined on the
basis of the matchers in `c2l-account-matchers-file'.  If none is
found, the value of `c2l-fallback-balancing-account' is used.  If
that option is unset, the user is asked for an account."
  (let* ((fields (--remove (eq (car it) '_) (-zip-pair c2l-csv-columns row)))
         (account (or (-some #'c2l--match-account (mapcar #'cdr (--filter (memq (car it) c2l-balancing-match-fields) fields)))
                      c2l-fallback-balancing-account
                      (completing-read (format "Account for transaction %s, %s «%.75s» "
                                               (funcall c2l-title-function fields)
                                               (alist-get 'amount fields)
                                               (alist-get 'description fields))
                                       c2l--accounts))))
    (c2l--compose-entry fields account)))

(defun c2l--get-current-row ()
  "Read the current line as a CSV row.
Return value is a list of values as strings."
  (let ((separator (car csv-separator-chars))
        (quote-char (string-to-char (or (car csv-field-quotes) "")))
        (line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (parse-csv-string line separator quote-char)))

(defun c2l--has-header ()
  "Return non-nil if the current CSV buffer appears to have a header.
Essentially, this function just takes the field that should
contain the amount and checks if it contains something that looks
like a number."
  (save-mark-and-excursion
    (goto-char (point-min))
    (let* ((row (c2l--get-current-row))
           (fields (--remove (eq (car it) '_) (-zip-pair c2l-csv-columns row))))
      (not (string-match-p "[0-9]+[0-9.,]*[.,][0-9]\\{2\\}"  (alist-get 'amount fields))))))

(defun c2l--get-results-buffer ()
  "Create a results buffer for conversion.
The buffer is called \"*Csv2Ledger Results*\".  If a buffer with
this name already exists, it is erased and returned.  Otherwise a
new buffer is created."
  (if (and c2l--results-buffer
           (buffer-live-p c2l--results-buffer))
      (with-current-buffer c2l--results-buffer
        (erase-buffer))
    (setq c2l--results-buffer (get-buffer-create "*Csv2Ledger Results*"))
    (when (fboundp 'ledger-mode)
      (with-current-buffer c2l--results-buffer
        (ledger-mode))))
  c2l--results-buffer)

;;;###autoload
(defun c2l-set-base-account ()
  "Set `c2l-base-account'."
  (unless c2l--accounts
    (setq c2l--accounts (c2l--read-accounts c2l-accounts-file)))
  (setq c2l-base-account (completing-read "Base account for current buffer: " c2l--accounts)))

;;;###autoload
(defun c2l-csv-entry-as-kill ()
  "Convert the current CSV row to a Ledger entry and place it in the kill ring.
The fields in the row are interpreted according to the template
in `c2l-csv-columns'."
  (interactive)
  (unless c2l--accounts
    (setq c2l--accounts (c2l--read-accounts c2l-accounts-file)))
  (let ((entry (c2l--csv-line-to-ledger (c2l--get-current-row))))
    (kill-new entry)
    (message entry)))

;;;###autoload
(defun c2l-convert-region (start end)
  "Convert the CSV entries in the region to ledger entries.
START and END describe the region.  Note that it is assumed that
START does indeed refer to the start of the region and END to its
end.  In other words, START must be smaller than END.  START and
END do not have to point to the start of end of a line, but the
conversion always takes the whole line into account.

This function always returns nil.  The converted entries are
placed in the buffer \"*Csv2Ledger Results*\", which is erased
beforehand if it already exists."
  (interactive "r")
  (let ((buffer (c2l--get-results-buffer))
        (n 0))
    (save-mark-and-excursion
      (goto-char start)
      (beginning-of-line)
      (while (< (point) end)
        (let ((entry (c2l--csv-line-to-ledger (c2l--get-current-row))))
          (setq n (1+ n))
          (with-current-buffer buffer
            (insert entry "\n")))
        (forward-line 1)))
    (message "[Csv2Ledger] Converted %d entries." n)))

;;;###autoload
(defun c2l-convert-buffer ()
  "Convert the CSV entries in the current buffer to ledger entries.
The converted entries are placed in the buffer \"*Csv2Ledger
Results*\", which is erased beforehand if it already exists.  If
the first line of the buffer is a header line, it is
skipped.  (The first line is considered to be a header if no
amount can be found in the amount column.)"
  (interactive)
  (let ((beg (save-mark-and-excursion
               (goto-char (point-min))
               (if (c2l--has-header)
                   (forward-line 1))
               (point))))
    (c2l-convert-region beg (point-max))))

(provide 'csv2ledger)

;;; csv2ledger.el ends here
