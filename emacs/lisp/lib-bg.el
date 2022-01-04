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

(defun bg-balance-data-max-convive-length (data)
  "Calculate the length of the longest convive name in DATA."
  (seq-max
   (seq-map
    #'length
    (seq-map #'vulpea-note-title
             (bg-balance-data-convives data)))))

(defun bg-balance-data-max-balance-length (data)
  "Calculate the length of the longest balance amount in DATA."
  (seq-max
   (cons (+ 1
            (length (number-to-string
                     (bg-balance-data-total data)))
            (length bg-currency))
         (seq-map
          #'length
          (seq-map #'cdr
                   (bg-balance-data-balances data))))))

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
         (name-max-length (+ 2 (bg-balance-data-max-convive-length data)))
         (balance-max-length (+ 1
                                (seq-max
                                 (seq-map
                                  #'length
                                  (seq-map
                                   #'number-to-string
                                   (cons
                                    (bg-balance-data-total data)
                                    (seq-map #'cdr (bg-balance-data-balances data))))))
                                (length bg-currency)))
         (name "*Barberry Garden Balance*")
         (buffer (buffer-generate name 'unique)))
    (with-current-buffer buffer
      (insert
       (bg-balance--format-row
        :item (propertize "Total" 'face 'bold)
        :item-width name-max-length
        :value (bg-balance-data-total data)
        :value-width balance-max-length)
       "\n\n")
      (seq-each
       (lambda (acc)
         (insert
          (bg-balance--format-row
           :item (concat "- " (vulpea-note-title acc))
           :item-width name-max-length
           :value (or (assoc-default (vulpea-note-id acc)
                                     (bg-balance-data-balances data))
                      0)
           :value-width balance-max-length)
          "\n"))
       (bg-balance-data-convives data))
      (insert "\n"
              (propertize "Latest transactions" 'face 'bold)
              "\n\n")
      (seq-each
       (lambda (p)
         (let* ((lp (concat
                     "- "
                     (propertize
                      (bg-posting-date p)
                      'face 'shadow)
                     ": "
                     (if (vulpea-note-p (bg-posting-account p))
                         (vulpea-note-title (bg-posting-account p))
                       (bg-posting-description p))))
                (rp (concat
                     (bg-balance--format-amount (bg-posting-amount p))
                     " -> "
                     (bg-balance--format-amount (bg-posting-total p))))
                (len 60))
           (insert lp
                   " "
                   (s-pad-left (- len (length lp)) " " rp)
                   "\n")))
       (seq-reverse
        (bg-balance-data-postings data)))
      (read-only-mode))
    (switch-to-buffer buffer)))

(defun bg-balance--format-amount (amount)
  "Format balance represented as AMOUNT."
  (let* ((value (concat (number-to-string amount) " " bg-currency)))
    (if (>= amount 0)
        (propertize value 'face 'success)
      (propertize value 'face 'error))))

(cl-defun bg-balance--format-row (&key item
                                       item-width
                                       value
                                       value-width)
  "Format ITEM balance represented as VALUE.

ITEM-WIDTH and VALUE-WIDTH are used to pad ITEM on the right and
VALUE on the left respectively."
  (concat
   (s-pad-right (+ item-width 2) " " (concat item ": "))
   (s-pad-left value-width " " (bg-balance--format-amount value))))



(provide 'lib-bg)
;;; lib-bg.el ends here
