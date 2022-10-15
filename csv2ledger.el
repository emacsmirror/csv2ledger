;;; csv2ledger.el --- Convert csv files to ledger entries  -*- lexical-binding: t -*-

;; Copyright (c) 2018 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2018
;; Version: 1.0

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; These are functions for personal use with ledger-cli and ledger-mode.

;;; Code:

(require 'ledger-mode)
(require 'parse-csv)
(require 'csv-mode)
(require 'dash)

(defvar ledger-post-amount-alignment-column)

(defvar c2l-accounts-file nil)
(defvar c2l-accounts nil)
(defvar c2l-account-matchers-file nil)
(defvar c2l-compiled-account-regexes nil)
(defvar c2l-default-to-account nil)
(defvar c2l-default-from-account nil)
(defvar c2l-account-holder nil)
(defvar c2l-csv-columns '(Date Valuation Type Description Sender Payee Amount Balance))

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
  (or to (setq to c2l-default-to-account))
  (setq amount (c2l-parse-amount amount))
  (let ((width ledger-post-amount-alignment-column))
    (concat (c2l-parse-date date) " * " title "\n"
            (if description (format "    ; Desc: %s\n" description) "")
            (format "    %s\n" (or from c2l-default-from-account))
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

(defun c2l-compile-account-regexes (accounts)
  "Group elements in ACCOUNTS according to account.
ACCOUNTS is a list of (<matcher> . <account>) conses, where
<matcher> should be unique but <account> may occur multiple
times.  Return value is a list in which the items in ACCOUNTS are
grouped by <account>."
  (mapcar (lambda (e)
            (cons (car e)
                  (regexp-opt (mapcar #'car (cdr e)))))
          (seq-group-by #'cdr accounts)))

(defun c2l-match-account (str)
  "Try to match STR to an account."
  (unless c2l-compiled-account-regexes
    (setq c2l-compiled-account-regexes
          (-> c2l-account-matchers-file
              (c2l-read-account-matchers)
              (c2l-compile-account-regexes))))
  (--some (if (string-match-p (cdr it) str)
              (car it))
          c2l-compiled-account-regexes))

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
                            (completing-read (format "Account for transaction %s, %s «%.75s» " title amount description) c2l-accounts nil t)))
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
  (unless c2l-accounts
    (setq c2l-accounts (c2l-read-accounts c2l-accounts-file)))
  (let* ((separator (car csv-separator-chars))
         (quote-char (string-to-char (or (car csv-field-quotes) "")))
         (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (row (parse-csv-string line separator quote-char))
         (entry (c2l-csv-line-to-ledger row)))
    (kill-new entry)
    (message entry)))

(provide 'csv2ledger)

;;; csv2ledger.el ends here
