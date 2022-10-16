;;; csv2ledger.el --- Convert csv files to ledger entries  -*- lexical-binding: t -*-

;; Copyright (c) 2022 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2022
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

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

(require 'ledger-mode)
(require 'parse-csv)
(require 'csv-mode)
(require 'dash)

(defvar ledger-post-amount-alignment-column)

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

(defcustom c2l-account-matchers-file nil
  "File containing matcher strings mapped to accounts.
This should be a TSV (tab-separated values) file containing one
matcher per line:

aldi          Expenses:Groceries
lidl          Expenses:Groceries
restaurant    Expenses:Leisure:Restaurant

where the two columns are separated by a TAB.

The matcher is a string (not a regular expression).  If the
matcher is found in the description, payee, type or sender of a
transaction (in that order), the corresponding account is used to
book the transaction."
  :type 'file
  :group 'csv2ledger)

(defcustom c2l-base-account nil
  "Base ledger account.
A CSV file normally lists transactions for a single bank account.
The base ledger account is the ledger account associated with
this bank account.  As such, it is the account that will turn up
in every transaction read from the CSV file.

This is a buffer-local variable and can be set in the local
variable block of a CSV file.  If its value is nil when invoking
`csv2ledger', the user is asked for a value.  Setting a default
value is useful if one only has one back account from which CSV
files are processed."
  :type 'file
  :group 'csv2ledger
  :local t)

(defcustom c2l-fallback-account nil
  "Fallback account for ledger transactions.
When creating a ledger entry, csv2ledger tries to determine the
opposite account for the transaction based on the matchers in
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

(defcustom c2l-csv-columns '(Date Valuation Type Description Sender Payee Amount _)
  "List of columns in the CSV file.
The data in the CSV file is extracted based on this list.  The
order of elements in the list should therefore represent the
order of columns in the CSV file."
  :type '(repeat symbol)
  :group 'csv2ledger)

(defvar c2l--accounts nil "List of ledger accounts, mainly used for completion.")
(defvar c2l--compiled-matcher-regexes nil "Alist of accounts and their matchers.")

(defun c2l-parse-date (date)
  "Convert DATE from \"17.10.2022\" to \"2022-10-17\"."
  (string-join (nreverse (split-string date "\\.")) "-"))

(defun c2l-parse-amount (amount)
  "Convert AMOUNT from \"-3.150,20 €\" to \"-€3150.20\"."
  (string-match "\\(-\\)?\\([[:digit:].]+\\),\\([[:digit:]]\\{2\\}\\)" amount)
  (let ((sign (or (match-string 1 amount) ""))
        (euros (string-replace "." "" (match-string 2 amount)))
        (cents (match-string 3 amount)))
    (concat sign "€" euros "." cents)))

(defun c2l-compose-entry (date title amount &optional description from to)
  "Create a ledger entry.
DATE, TITLE, AMOUNT are used to create the entry.  DESCRIPTION,
if non-nil, is added as a comment, preceded by \"Desc:\".  FROM
is the account where the money comes from, TO the account to
which it goes.  Note that if AMOUNT is negative, these roles are
reversed."
  ;; TODO Fix doc string
  (or to (setq to c2l-base-account))
  (setq amount (c2l-parse-amount amount))
  (let ((width ledger-post-amount-alignment-column))
    (concat (c2l-parse-date date) " * " title "\n"
            (if description (format "    ; Desc: %s\n" description) "")
            (format "    %s\n" (or from c2l-fallback-account))
            (format "    %s  " to)
            (make-string (- width 4 (length to) 2 (length amount)) ?\s)
            amount "\n")))

(defun c2l-read-accounts (file)
  "Read list of accounts from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (accounts)
      (while (not (eobp))
        (if (looking-at "^account \\([[:print:]]+\\)$")
            (push (match-string 1) accounts))
        (forward-line 1))
      accounts)))

(defun c2l-read-account-matchers (file)
  "Read account matchers from FILE.
FILE should be a tsv file with one matcher per line, with the
matcher in column 1 and the associated account in column 2.

A matcher is a string (not a regular expression).  If the string
is found in the description, payee, type of sender of a
transaction (in that order), the account with the matcher is
taken as the account to book the transaction to."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (accounts)
      (while (looking-at "\\([[:print:]]+\\)\t\\([[:print:]]+\\)")
        (let ((matcher (match-string 1))
              (account (match-string 2)))
          (push (cons matcher account) accounts))
        (forward-line 1))
      accounts)))

(defun c2l-compile-matcher-regexes (accounts)
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

(defun c2l-match-account (str)
  "Try to match STR to an account."
  (unless c2l--compiled-matcher-regexes
    (setq c2l--compiled-matcher-regexes
          (-> c2l-account-matchers-file
              (c2l-read-account-matchers)
              (c2l-compile-matcher-regexes))))
  (--some (if (string-match-p (cdr it) str)
              (car it))
          c2l--compiled-matcher-regexes))

(defun c2l-csv-line-to-ledger (row)
  "Convert ROW to a ledger entry.
ROW is a list of strings and should have the following elements:

Date Valuation Type Description Sender Payee Amount Balance

Valuation and Balance are ignored, the other elements are used to
create the ledger entry."
  (pcase-let* ((`(,date ,_ ,type ,description ,sender ,payee ,amount ,_) row)
               (title (if (and (stringp c2l-account-holder)
                               (string-match-p c2l-account-holder payee))
                          sender
                        payee))
               (account (or (seq-some #'c2l-match-account (list description payee type sender))
                            (completing-read (format "Account for transaction %s, %s «%.75s» " title amount description) c2l--accounts nil t)))
               (entry (c2l-compose-entry date title amount description account)))
    entry))

;;;###autoload
(defun c2l-csv-entry-as-kill ()
  "Convert the current csv row to a Ledger entry and place it in the kill ring.
The line should have the following format:

Date Valuation Type Description Sender Payee Amount Balance

Valuation and Balance are ignored, the other elements are used to
create the ledger entry."
  (interactive)
  (unless c2l--accounts
    (setq c2l--accounts (c2l-read-accounts c2l-accounts-file)))
  (let* ((separator (car csv-separator-chars))
         (quote-char (string-to-char (or (car csv-field-quotes) "")))
         (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (row (parse-csv-string line separator quote-char))
         (entry (c2l-csv-line-to-ledger row)))
    (kill-new entry)
    (message entry)))

(provide 'csv2ledger)

;;; csv2ledger.el ends here
