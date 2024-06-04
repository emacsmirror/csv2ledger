;;; csv2ledger.el --- Convert csv files to ledger entries  -*- lexical-binding: t -*-

;; Copyright (c) 2023-2024 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2022
;; Version: 1.5.3
;; Package-Requires: ((emacs "29.1") (csv-mode "1.24"))
;; URL: https://codeberg.org/joostkremers/csv2ledger

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

;; This package aims to make it easier to convert CSV files with bank statements
;; to Ledger entries.
;;
;; To get started, you will at the very least need to set `c2l-base-account' and
;; `c2l-csv-columns'.  With these variables set up, you can try to convert a CSV
;; file with the commands `c2l-convert-buffer' or `c2l-convert-region'.  These
;; commands will convert each transaction in the buffer or region, asking you
;; for each which balancing (or target) account to use.  Make sure to set
;; `c2l-account-file' to the ledger file that contains your account definitions,
;; so that when Emacs asks you for an account, it can offer the accounts in your
;; ledger file as completion targets.
;;
;; For more convenience, set up a file with account matchers and point the
;; variable `c2l-account-matchers-file' to it.  This will allow `csv2ledger' to
;; try and find a target account for each transaction automatically without user
;; intervention.  Also look at the variable `c2l-target-match-fields' to improve
;; account matching for your CSV files.  Additionally, you can set
;; `c2l-fallback-account' if you do not wish to be asked for a target account
;; for each transaction for which `csv2ledger' could not find one and instead
;; prefer to correct the resulting ledger entries afterwards.
;;
;; Normally, a ledger entry starts with a date and the payee, but if you receive
;; money, your CSV file may list you as the payee.  To use the sender instead in
;; such cases, set the variable `c2l-account-holder' to a regular expression
;; matching your name (i.e., whatever appears as the payee in your CSV file when
;; you receive money).
;;
;; If you need to make any kind of changes to the data in your CSV file or wish
;; to customise the format of your ledger entries, look at the variables
;; `c2l-field-modify-functions', `c2l-transaction-modify-functions' and
;; `c2l-entry-function'.
;;
;; Lastly, the variable `c2l-auto-cleared' controls whether transactions are
;; marked as "cleared" or not, and `c2l-alignment-column' controls the position
;; of the amount in the transaction.
;;
;; For more detailed information, see the README at
;; <https://codeberg.org/joostkremers/csv2ledger>.

;;; Code:

(require 'subr-x)
(require 'csv-mode)

(defgroup csv2ledger nil
  "Csv2Ledger: Converting csv files to ledger entries."
  :group 'ledger
  :group 'CSV)

(defcustom c2l-accounts-file nil
  "File containing account declarations.
This should point to a ledger file that defines the accounts.  It
can be a separate file or a ledger file containing transactions."
  :type 'file
  :safe #'stringp
  :group 'csv2ledger)

(defcustom c2l-base-account "Assets:Checking"
  "Base ledger account.
A CSV file normally lists transactions for a single bank account.
The base ledger account is the ledger account associated with
this bank account.  As such, it is the account that will turn up
in every transaction read from the CSV file."
  :type 'file
  :safe #'stringp
  :group 'csv2ledger)

(defcustom c2l-fallback-account nil
  "Fallback for the target or balancing account in transactions.
When creating a ledger entry, csv2ledger tries to determine the
target account for the transaction based on the matchers in
`c2l-account-matchers-file'.  If no acccount is found, the value
of this variable is used.  If the value is unset, the user is
asked for an account."
  :type 'string
  :safe #'stringp
  :group 'csv2ledger)

(defcustom c2l-account-holder nil
  "Regular expression matching the account holder.
If the payee matches this regular expression, the sender is used
instead of the payee as the title of the entry."
  :type 'string
  :safe #'stringp
  :group 'csv2ledger)

;;;###autoload (put 'c2l-csv-columns 'safe-local-variable '(lambda (v) (seq-every-p #'stringp v)))
(defcustom c2l-csv-columns '()
  "List of columns in the CSV file.
The data in the CSV file is extracted based on this list.  The
order of elements in the list should therefore represent the
order of columns in the CSV file.  A column that is not relevant
can be labeled with an underscore.

Valid column names are the following:

- `date': booking date of the transaction
- `posted': effective date of the transaction
- `description': whatever the bank provides
- `sender': the initiator of the payment
- `payee': the party receiving the payment
- `amount': the amount of the payment (positive or negative)
- `credit': the amount received
- `debit': the amount payed

Other column names can be added, but they cannot be used directly
in the transaction.  They may be used in the option
`c2l-target-match-fields' or in custom functions for
`c2l-transaction-modify-functions', however."
  :type '(repeat symbol)
  :group 'csv2ledger)

(defcustom c2l-field-modify-functions nil
  "List of functions to modify fields in an entry.
This option should be an alist mapping field names (as symbols)
to functions.  These functions should take a single string
argument and should return a string, which will be the value used
for the field in question."
  :type '(repeat (cons (symbol :tag "Field") function))
  :group 'csv2ledger)

(defcustom c2l-transaction-modify-functions '(c2l-create-title c2l-create-amount c2l-create-account)
  "List of functions applied to the transaction before creating an entry.
The functions are applied in the order in which they appear in
the list.  Each function should take an alist representing a
transaction as argument and should return the modified
transaction.

These functions are composed into a single function which is then
stored in the variable `c2l-transaction-modifier'.  If you set
this option outside of Customize, make sure to call the function
`c2l-compose-transaction-modifier' as well."
  :type '(repeat function)
  :safe (lambda (v)
          (seq-every-p #'symbolp v))
  :group 'csv2ledger)

(defvar-local c2l-transaction-modifier nil
  "The function that modifies a CSV transaction before creating a ledger entry.
This is the composite function created with the functions in
`c2l-transaction-modify-functions'.")

(defun c2l-compose-transaction-modifier ()
  "Function to set the variable `c2l-transaction-modifier'."
  (or c2l-transaction-modifier
      (setq c2l-transaction-modifier (lambda (acc)
                                       (dolist (fn c2l-transaction-modify-functions acc)
                                         (setq acc (funcall fn acc)))))))

(defcustom c2l-entry-function #'c2l-compose-entry
  "Function to create a ledger entry.
This should be a function that takes an alist of field-value
pairs and returns a string.  The string should be a formatted
Ledger entry."
  :type 'function
  :safe #'functionp
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
is found in any of the fields listed in the option
`c2l-target-match-fields', the corresponding account is used to
book the transaction."
  :type 'file
  :safe #'stringp
  :group 'csv2ledger)

(defvar c2l-matcher-regexps nil
  "Alist of matcher regexps and their accounts.
Each item should be a cons cell of a regular expression and an
account name.  If the regular expression matches any of the
fields in `c2l-target-match-fields', its corresponding account is
used as the target account.

This variable is normally given a value based on the matchers in
`c2l-account-matchers-file', but it can also be set directly.")

(defun c2l--read-account-matchers-file (file)
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
            (nreverse accounts)))
      (user-error "[Csv2Ledger] Account matcher file `%s' not found" file))))

(defun c2l--compile-matcher-regexps (accounts)
  "Create efficient regular expressions for the matchers in ACCOUNTS.
ACCOUNTS is a list of (<matcher> . <account>) conses, where
<matcher> should be unique but <account> may occur multiple
times.  Return value is an alist in which each account in
ACCOUNTS is mapped to a regular expression matching all matchers
for that account."
  (mapcar (lambda (e)
            (cons (regexp-opt (mapcar #'car (cdr e)))
                  (car e)))
          (seq-group-by #'cdr accounts)))

(defun c2l-set-matcher-regexps (&optional force)
  "Set `c2l-matcher-regexps' based on the file in `c2l-account-matchers-file'.
If `c2l-matcher-regexps' already has a value, do not change it,
unless FORCE is non-nil.  Return the (new) value of
`c2l-matcher-regexps'."
  (when (or force
            (not c2l-matcher-regexps))
    (setq-local c2l-matcher-regexps
                (thread-first c2l-account-matchers-file
                              (c2l--read-account-matchers-file)
                              (c2l--compile-matcher-regexps))))
  c2l-matcher-regexps)

;;;###autoload (put 'c2l-target-match-fields 'safe-local-variable '(lambda (v) (seq-every-p #'stringp v)))
(defcustom c2l-target-match-fields '(payee description)
  "List of fields used for determining the target account.
Fields in this list are matched against the matchers in
`c2l-account-matchers-file'.  Note that the order of the fields
in this list can be relevant, because the first field that
returns a match is used as the target account."
  :type '(repeat symbol)
  :group 'csv2ledger)

(defcustom c2l-auto-cleared nil
  "If non-nil, mark every transaction as cleared."
  :type '(choice (const :tag "Never auto-clear transactions" nil)
                 (const :tag "Always auto-clear transactions" t)
                 (const :tag "Auto-clear if effective date is present" if-posted))
  :safe #'symbolp
  :group 'csv2ledger)

(defcustom c2l-alignment-column 52
  "The column to which amounts are aligned.
This should most likely be set to the same value as
`ledger-post-amount-alignment-column'."
  :type 'integer
  :safe #'integerp
  :group 'csv2ledger)

(defvar-local c2l--accounts nil "List of ledger accounts, mainly used for completion.")
(defvar c2l--results-buffer nil "Buffer for conversion results.")

;;; Functions for use as values of customisation options.

(defun c2l-convert-little-endian-to-iso8601-date (date)
  "Convert DATE from a little-endian format to an ISO 8601 format.
DATE should be a string representing a date of the form
DD.MM.YYYY, DD/MM/YYYY or DD-MM-YYYY.  Return value is a date
string of the form YYYY-MM-DD.

Note that the input date may have dots, dashes or forward slashes
separating the date parts; also, additional whitespace is
removed.  This function does not check if DATE has a valid date
format, it just splits DATE on the separator, reverses the date
parts and joins them again, using a hyphen as separator."
  (string-join (nreverse (split-string date "[./-]" t "[[:space:]]")) "-"))

(defun c2l-create-title (transaction)
  "Create a title for TRANSACTION.
Return the modified transaction."
  (let* ((payee (alist-get 'payee transaction ""))
         (sender (alist-get 'sender transaction ""))
         (title (cond
                 ((and (string-empty-p payee)
                       (string-empty-p sender))
                  "Unknown payee")
                 ((string-empty-p payee) sender)
                 ((string-empty-p sender) payee)
                 ((and (stringp c2l-account-holder)
                       (string-match-p c2l-account-holder payee))
                  sender)
                 (t payee))))
    (push (cons 'title title) transaction)
    transaction))

(defun c2l-create-amount (transaction)
  "Create the amount for TRANSACTION.
Return the modified transaction."
  (unless (c2l--amount-p (alist-get 'amount transaction ""))
    (let ((amount (or (c2l--amount-p (alist-get 'credit transaction ""))
                      (c2l--amount-p (alist-get 'debit transaction ""))
                      "0.00")))
      (if (alist-get 'amount transaction)
          (setf (alist-get 'amount transaction) amount)
        (push (cons 'amount amount) transaction))))
  transaction)

(defun c2l-create-account (transaction)
  "Get the balancing account for TRANSACTION.
First check if the account matchers provide a balancing account.
If not, use `c2l-fallback-account' if provided.  If neither
method yields an account, ask the user."
  (let ((account (or (seq-some #'c2l--match-account
                               (mapcar #'cdr
                                       (seq-filter (lambda (e) (memq (car e) c2l-target-match-fields))
                                                   transaction)))
                     c2l-fallback-account
                     (completing-read (format "Account for transaction %s, %s «%.75s» "
                                              (alist-get 'title transaction "Unknown payee")
                                              (alist-get 'amount transaction "0.00")
                                              (alist-get 'description transaction "?"))
                                      c2l--accounts))))
    (push (cons 'account account) transaction)
    transaction))

(defun c2l-create-account-ask-matcher (transaction)
  "Get the balancing account for TRANSACTION.
First check if the account matchers provide a balancing account.
If not, ask the user for an account and ask if they want to
create an account matcher for it."
  (let ((account (seq-some #'c2l--match-account
                           (mapcar #'cdr
                                   (seq-filter (lambda (e) (memq (car e) c2l-target-match-fields))
                                               transaction)))))
    (when (not account)
      (setq account (completing-read (format "Account for transaction %s, %s «%.75s» "
                                             (alist-get 'title transaction "Unknown payee")
                                             (alist-get 'amount transaction "0.00")
                                             (alist-get 'description transaction "?"))
                                     c2l--accounts))
      (if (y-or-n-p "Add a matcher for this account? ")
          ;; If the user changes their mind, we still want to continue
          ;; processing the transaction, so we capture quit.
          (condition-case nil
              (let ((matcher (read-string (format "Matcher for «%s»: " account))))
                (when (and matcher (not (string-empty-p matcher)))
                  (c2l-add-matcher matcher account c2l-account-matchers-file)
                  (push (cons (regexp-opt (list matcher)) account) c2l-matcher-regexps)))
            (quit nil))))
    (push (cons 'account account) transaction)
    transaction))

(defun c2l-add-matcher (matcher account file)
  "Add MATCHER to FILE.
MATCHER will be linked to ACCOUNT."
  (if (not (and (stringp file)
                (file-writable-p file)))
      (user-error "[Csv2Ledger] Cannot write to account matchers file `%s'" file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (ensure-empty-lines 0)            ; Make sure we're on a new, empty line.
      (insert (format "%s\t%s\n" matcher account))
      (write-region (point-min) (point-max) file))))

(defun c2l-compose-entry (transaction)
  "Create a ledger entry.
TRANSACTION is an alist containing (key . value) pairs that will
be included in the entry.  It should at least contain values for
the keys `date', `title', `amount' and `account'.  TRANSACTION
may also contain a value for `posted' and `description'.  If
`posted' is present, it is added as the effective date for the
entry.  If `description' is present, it is added as a comment,
preceded by \"Desc:\".  If `c2l-auto-cleared' is non-nil, the
entry is marked as cleared."
  (let-alist transaction
    (let ((cleared (if (eq c2l-auto-cleared 'if-posted) .posted c2l-auto-cleared)))
      (concat .date (if .posted (format "=%s" .posted) "") (if cleared " * " " ") .title "\n"
              (if (and .description (not (string-empty-p .description))) (format "    ; Desc: %s\n" .description) "")
              (format "    %s\n" .account)
              (format "    %s  " c2l-base-account)
              (make-string (- c2l-alignment-column 4 (length c2l-base-account) 2 (length .amount)) ?\s)
              .amount "\n"))))

;;; Helper functions

(defun c2l--amount-p (str)
  "Return non-nil if STR is likely to be an amount."
  (if (string-match-p "[0-9]+[0-9.,]*\\(?:[.,][0-9]\\{2\\}\\)?" str)
      str))

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

(defun c2l--match-account (str)
  "Try to match STR to an account."
  (seq-some (lambda (e)
              (if (string-match-p (car e) str)
                  (cdr e)))
            (c2l-set-matcher-regexps)))

(defun c2l--csv-line-to-ledger (transaction)
  "Convert TRANSACTION to a ledger entry.
TRANSACTION is an alist containing the data of the transaction.
This function first applies the functions in
`c2l-field-modify-functions' to the individual fields of
TRANSACTION and then passes the transaction through
`c2l-transaction-modify-functions' before calling
`c2l-entry-function' to create the actual entry as a string."
  (let* ((modified-fields (mapcar (lambda (item)
                                    (let ((field (car item))
                                          (value (cdr item)))
                                      (cons field
                                            (funcall (alist-get field c2l-field-modify-functions #'identity) value))))
                                  transaction))
         (modified-transaction (funcall (c2l-compose-transaction-modifier) modified-fields)))
    (funcall c2l-entry-function modified-transaction)))

(defun c2l--get-current-row ()
  "Read the current line as a CSV row.
Return value is an alist of field-value pairs, where the field
names are taken from `c2l-csv-columns'."
  (if c2l-csv-columns
      (let ((row (csv-parse-current-row)))
        (seq-remove (lambda (e) (eq (car e) '_))
                    (seq-mapn #'cons c2l-csv-columns row)))
    (user-error "[Csv2Ledger] Cannot interpret CSV data; set `c2l-csv-columns' first")))

(defun c2l--has-header ()
  "Return non-nil if the current CSV buffer appears to have a header.
Essentially, this function just checks the `amount', `credit' and
`debit' fields on the first line of the buffer and returns nil if
one of these contains something that looks like an amount."
  (save-mark-and-excursion
    (goto-char (point-min))
    (let* ((transaction (c2l--get-current-row)))
      (not (seq-some (lambda (field)
                       (c2l--amount-p (alist-get field transaction "")))
                     '(amount credit debit))))))

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

;;; Interactive functions

;;;###autoload
(defun c2l-set-base-account ()
  "Set `c2l-base-account'."
  (unless c2l--accounts
    (setq c2l--accounts (c2l--read-accounts c2l-accounts-file)))
  (setq c2l-base-account (completing-read "Base account for current buffer: " c2l--accounts)))

(defun c2l-read-account-matchers ()
  "(Re)read the account matchers file."
  (interactive)
  (c2l-set-matcher-regexps 'force)
  (message "[Csv2Ledger] Reading account matchers file... Done."))

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
    (display-buffer buffer)
    (message "[Csv2Ledger] Converted %d entries." n)))

;;;###autoload
(defun c2l-convert-buffer ()
  "Convert the CSV entries in the current buffer to ledger entries.
The converted entries are placed in the buffer \"*Csv2Ledger
Results*\", which is erased beforehand if it already exists.  If
the first line of the buffer looks like a header line (see
`c2l--has-header'), it is skipped."
  (interactive)
  (let ((beg (save-mark-and-excursion
               (goto-char (point-min))
               (if (c2l--has-header)
                   (forward-line 1))
               (point))))
    (c2l-convert-region beg (point-max))))

(provide 'csv2ledger)

;;; csv2ledger.el ends here
