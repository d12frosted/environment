;;; lib-bg.el --- Barberry Garden helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 30 Dec 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Various utilities to help with Barberry Garden organisation.
;;
;;; Code:

(require 'vulpea)



(defvar bg-ledger-file nil
  "Path to Barberry Garden ledger file.")

(defvar bg-currency nil
  "Currency used in Barberry Garden.")



(cl-defun bg-record-txn (&key date comment account-to account-from amount)
  "Record transaction.

DATE (can be nil) is a time object as returned by `current-time'.

COMMENT (can be nil) is a transaction description.

ACCOUNT-TO is account that receives AMOUNT.

ACCOUNT-FROM is account that spends AMOUNT.

AMOUNT is number in `bg-currency'.

Transaction is recorded into `bg-ledger-file'."
  (shell-command-to-string
   (format
    "echo '\n%s%s\n    %s  %s %s\n    %s' >> '%s'"
    (format-time-string "%Y/%m/%d" date)
    (if comment
        (concat " " comment)
      "")
    account-to
    amount
    bg-currency
    account-from
    bg-ledger-file)))

;;;###autoload
(defun bg-balance-deposit ()
  "Deposit an amount for convive."
  (interactive)
  (let ((convive (vulpea-select-from
                  "People"
                  (vulpea-db-query-by-tags-some '("people"))
                  :require-match t))
        (amount (read-number "Amount: "))
        (date (org-read-date nil t)))
    (bg-record-txn
     :date date
     :comment "deposit"
     :account-to (concat "balance:" (vulpea-note-id convive))
     :account-from (concat "convive:" (vulpea-note-id convive))
     :amount amount)))

;;;###autoload
(defun bg-balance-charge ()
  "Charge an amount from convive."
  (interactive)
  (let ((convive (vulpea-select-from
                  "People"
                  (vulpea-db-query-by-tags-some '("people"))
                  :require-match t))
        (amount (read-number "Amount: "))
        (date (org-read-date nil t)))
    (bg-record-txn
     :date date
     :comment "charge"
     :account-to "balance:assets"
     :account-from (concat "balance:" (vulpea-note-id convive))
     :amount amount)))

;;;###autoload
(defun bg-balance-spend ()
  "Spend an amount on event."
  (interactive)
  (let ((amount (read-number "Amount: "))
        (date (org-read-date nil t))
        (comment (read-string "Comment: ")))
    (bg-record-txn
     :date date
     :comment comment
     :account-to "expenses"
     :account-from "balance:assets"
     :amount amount)))



(cl-defstruct bg-balance-data
  total
  convives
  balances
  postings)

(cl-defstruct bg-posting
  date
  description
  account
  amount
  total)

(defun bg-balance-data-read ()
  "Read balance data from `bg-ledger-file'."
  (let* ((prefix "balance:")
         (ignored '("assets"))

         (cmd-bal (format "hledger -f '%s' balance '%s'" bg-ledger-file prefix))
         (res-bal (split-string (shell-command-to-string cmd-bal) "\n" t " +"))
         (balances (seq-remove
                    (lambda (kvp)
                      (seq-contains-p ignored (car kvp)))
                    (seq-map
                     (lambda (x)
                       (let ((vs (split-string x prefix t " +")))
                         (cons
                          (cadr vs)
                          (string-to-number (car vs)))))
                     (-drop-last 2 res-bal))))
         (total (string-to-number (-last-item res-bal)))

         (cmd-accs (format "hledger -f '%s' accounts '%s'" bg-ledger-file prefix))
         (res-accs (shell-command-to-string cmd-accs))
         (convives (emacsql-with-transaction (org-roam-db)
                     (seq-map
                      #'vulpea-db-get-by-id
                      (seq-remove
                       (lambda (x)
                         (seq-contains-p ignored x))
                       (seq-map
                        (lambda (x)
                          (string-remove-prefix prefix x))
                        (split-string res-accs "\n" t))))))

         (cmd-register (format "hledger -f '%s' register -O csv -H '%s'"
                               bg-ledger-file prefix))
         (res-register (shell-command-to-string cmd-register))
         (postings (seq-map
                    (lambda (line)
                      (let* ((parts
                              (split-string-and-unquote line ","))
                             (account (string-remove-prefix prefix (nth 4 parts)))
                             (account (or (vulpea-db-get-by-id account)
                                          account)))
                        (make-bg-posting
                         :date (nth 1 parts)
                         :description (nth 3 parts)
                         :account account
                         :amount (string-to-number (nth 5 parts))
                         :total (string-to-number (nth 6 parts)))))
                    (cdr (split-string res-register "\n" t)))))
    (make-bg-balance-data
     :total total
     :convives convives
     :balances balances
     :postings postings)))

;;;###autoload
(defun bg-balance-display ()
  "Display Barberry Garden balance."
  (interactive)
  (let* ((data (bg-balance-data-read))
         (name "*Barberry Garden Balance*")
         (buffer (buffer-generate name 'unique)))
    (with-current-buffer buffer
      (insert
       (propertize "Balance" 'face 'bold)
       "\n\n"
       (string-table
        :data
        (cons
         (list "Total"
               (bg-balance--format-amount (bg-balance-data-total data)))
         (seq-map
          (lambda (acc)
            (list
             (vulpea-note-title acc)
             (bg-balance--format-amount
              (or (assoc-default (vulpea-note-id acc)
                                 (bg-balance-data-balances data))
                  0)
              :positive-face 'warning
              :zero-face 'success)))
          (bg-balance-data-convives data)))
        :row-start "- "
        :sep "  ")
       "\n"
       (propertize "Latest transactions" 'face 'bold)
       "\n\n"
       (string-table
        :data (seq-map
               (lambda (p)
                 (list
                  (propertize (bg-posting-date p) 'face 'shadow)
                  (if (vulpea-note-p (bg-posting-account p))
                      (vulpea-note-title (bg-posting-account p))
                    (bg-posting-description p))
                  (bg-balance--format-amount (bg-posting-amount p))
                  "->"
                  (bg-balance--format-amount (bg-posting-total p))))
               (seq-reverse
                (bg-balance-data-postings data)))
        :row-start "- "
        :sep "  "))
      (read-only-mode))
    (switch-to-buffer buffer)))

(cl-defun bg-balance--format-amount (amount
                                     &key
                                     positive-face
                                     zero-face
                                     negative-face)
  "Format balance represented as AMOUNT.

Uses POSITIVE-FACE, ZERO-FACE and NEGATIVE-FACE for prettifying."
  (let* ((value (concat (number-to-string amount) " " bg-currency))
         (face (cond
                ((> amount 0)
                 (or positive-face 'success))
                ((< amount 0)
                 (or negative-face 'error))
                (t
                 (or zero-face 'warning)))))
    (propertize value 'face face)))



(provide 'lib-bg)
;;; lib-bg.el ends here
