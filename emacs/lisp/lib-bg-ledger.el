;;; lib-bg-ledger.el --- Barberry Garden Ledger -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
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
;; NB! These functions are tied to specific use case. Feel free to
;; copy them, and modify as you wish. But don't expect stability or
;; new features.
;;
;; A set of helpers to maintain Barberry Garden ledger. There are 4
;; interesting interactive functions.
;;
;; - `bg-ledger-display' - display ledger. Includes total balance,
;;   balance for each participant and list of transactions. Many other
;;   interactive functions work in a DWIM fashion when used in this
;;   buffer.
;;
;; - `bg-ledger-spend' - spend some amount. Modifies total balance.
;;
;; - `bg-ledger-charge' - charge a person for some amount. Modifies
;;   balance of specific participant (subtract). Useful to track who paid what.
;;
;; - `bg-ledger-deposit' - deposit some amount for a person. Modifies
;;   balance of specific participant AND total balance.
;;
;; P.S. Person, participant and convive are used as synonyms in this
;; file.
;;
;; See https://barberry.io for more information about the project.
;;
;;; Code:

(require 'vulpea)
(require 'lib-buffer)



(defvar bg-ledger-file nil
  "Path to Barberry Garden ledger file.")

(defvar bg-ledger-currency nil
  "Currency used in Barberry Garden.")

(defvar bg-ledger-buffer-name "*Barberry Garden Balance*"
  "Name of balance buffer.")



(cl-defun bg-ledger-record-txn (&key date comment account-to account-from amount)
  "Record transaction.

DATE (can be nil) is a time object as returned by `current-time'.

COMMENT (can be nil) is a transaction description.

ACCOUNT-TO is account that receives AMOUNT.

ACCOUNT-FROM is account that spends AMOUNT.

AMOUNT is number in `bg-ledger-currency'.

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
    bg-ledger-currency
    account-from
    bg-ledger-file)))

;;;###autoload
(defun bg-ledger-deposit ()
  "Deposit an amount for convive."
  (interactive)
  (let* ((name (seq-find
                (lambda (str)
                  (and (not (s-matches-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
                       (not (s-suffix-p bg-ledger-currency str))))
                (s-split
                 "  "
                 (s-chop-prefix "- " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 t)))
         (convive (vulpea-select-from
                   "People"
                   (vulpea-db-query-by-tags-some '("people"))
                   :require-match t
                   :initial-prompt name))
         (data (bg-ledger-data-read))
         (balance (assoc-default (vulpea-note-id convive) (bg-ledger-data-balances data)))
         (amount (read-number "Amount: " (when balance (* -1 balance))))
         (date (org-read-date nil t)))
    (bg-ledger-record-txn
     :date date
     :comment "deposit"
     :account-to (concat "balance:" (vulpea-note-id convive))
     :account-from (concat "convive:" (vulpea-note-id convive))
     :amount amount)
    (bg-ledger-buffer-create)))

;;;###autoload
(defun bg-ledger-charge ()
  "Charge an amount from convive."
  (interactive)
  (let* ((name (seq-find
                (lambda (str)
                  (and (not (s-matches-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
                       (not (s-suffix-p bg-ledger-currency str))))
                (s-split
                 "  "
                 (s-chop-prefix "- " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 t)))
         (convive (vulpea-select-from
                   "People"
                   (vulpea-db-query-by-tags-some '("people"))
                   :require-match t
                   :initial-prompt name))
         (amount (read-number "Amount: "))
         (date (org-read-date nil t)))
    (bg-ledger-record-txn
     :date date
     :comment "charge"
     :account-to "balance:assets"
     :account-from (concat "balance:" (vulpea-note-id convive))
     :amount amount)
    (bg-ledger-buffer-create)))

;;;###autoload
(defun bg-ledger-spend ()
  "Spend an amount on event."
  (interactive)
  (let ((amount (read-number "Amount: "))
        (date (org-read-date nil t))
        (comment (read-string "Comment: ")))
    (bg-ledger-record-txn
     :date date
     :comment comment
     :account-to "expenses"
     :account-from "balance:assets"
     :amount amount)
    (bg-ledger-buffer-create)))



(cl-defstruct bg-ledger-data
  total
  convives
  balances
  postings)

(cl-defstruct bg-ledger-posting
  date
  description
  account
  amount
  total)

(defun bg-ledger-data-read ()
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
                        (make-bg-ledger-posting
                         :date (nth 1 parts)
                         :description (nth 3 parts)
                         :account account
                         :amount (string-to-number (nth 5 parts))
                         :total (string-to-number (nth 6 parts)))))
                    (cdr (split-string res-register "\n" t)))))
    (make-bg-ledger-data
     :total total
     :convives convives
     :balances balances
     :postings postings)))

(defun bg-ledger-buffer-create ()
  "Create ledger BUFFER and fill it with relevant information.

Return generated buffer."
  (let ((data (bg-ledger-data-read))
        (buffer (or (get-buffer bg-ledger-buffer-name)
                    (buffer-generate bg-ledger-buffer-name 'unique))))
    (with-current-buffer buffer
      (result-present-mode -1)
      (save-excursion
        (delete-region (point-min) (point-max))
        (insert
         (string-join
          (list (propertize "Balance" 'face 'bold)
                ""
                (string-table
                 :data
                 (cons
                  (list "Total"
                        (bg-ledger--format-amount (bg-ledger-data-total data)))
                  (seq-map
                   (lambda (acc)
                     (list
                      (vulpea-note-title acc)
                      (bg-ledger--format-amount
                       (or (assoc-default (vulpea-note-id acc)
                                          (bg-ledger-data-balances data))
                           0)
                       :positive-face 'warning
                       :zero-face 'success)))
                   (bg-ledger-data-convives data)))
                 :row-start "- "
                 :sep "  ")
                ""
                (propertize "Latest transactions" 'face 'bold)
                ""
                (string-table
                 :data (seq-map
                        (lambda (p)
                          (list
                           (propertize (bg-ledger-posting-date p) 'face 'shadow)
                           (if (vulpea-note-p (bg-ledger-posting-account p))
                               (vulpea-note-title (bg-ledger-posting-account p))
                             (bg-ledger-posting-description p))
                           (bg-ledger--format-amount (bg-ledger-posting-amount p))
                           "->"
                           (bg-ledger--format-amount (bg-ledger-posting-total p))))
                        (seq-reverse
                         (seq-remove
                          (lambda (p)
                            (string-equal "charge" (bg-ledger-posting-description p)))
                          (bg-ledger-data-postings data))))
                 :row-start "- "
                 :sep "  "))
          "\n")))
      (result-present-mode +1))
    buffer))

;;;###autoload
(defun bg-ledger-display ()
  "Display Barberry Garden ledger."
  (interactive)
  (let ((buffer (bg-ledger-buffer-create)))
    (switch-to-buffer buffer)))

(cl-defun bg-ledger--format-amount (amount
                                    &key
                                    positive-face
                                    zero-face
                                    negative-face)
  "Format balance represented as AMOUNT.

Uses POSITIVE-FACE, ZERO-FACE and NEGATIVE-FACE for prettifying."
  (let* ((value (concat (number-to-string amount) " " bg-ledger-currency))
         (face (cond
                ((> amount 0)
                 (or positive-face 'success))
                ((< amount 0)
                 (or negative-face 'warning))
                (t
                 (or zero-face 'warning)))))
    (propertize value 'face face)))



(provide 'lib-bg-ledger)
;;; lib-bg-ledger.el ends here
