;;; lib-brb-ledger.el --- Barberry Garden Ledger -*- lexical-binding: t; -*-
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
;; - `brb-ledger-display' - display ledger. Includes total balance,
;;   balance for each participant and list of transactions. Many other
;;   interactive functions work in a DWIM fashion when used in this
;;   buffer.
;;
;; - `brb-ledger-spend' - spend some amount. Modifies total balance.
;;
;; - `brb-ledger-charge' - charge a person for some amount. Modifies
;;   balance of specific participant (subtract). Useful to track who paid what.
;;
;; - `brb-ledger-deposit' - deposit some amount for a person. Modifies
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
(require 'lib-brb)



(defvar brb-ledger-file nil
  "Path to Barberry Garden ledger file.")

(defvar brb-ledger-buffer-name "*Barberry Garden Balance*"
  "Name of balance buffer.")



(cl-defun brb-ledger-record-txn (&key date code comment account-to account-from amount)
  "Record transaction.

DATE (can be nil) is a time object as returned by `current-time'.

CODE (can be nil) is idempotency key. When non-nil, any
transaction sharing provided CODE is replaced.

COMMENT (can be nil) is a transaction description.

ACCOUNT-TO is account that receives AMOUNT.

ACCOUNT-FROM is account that spends AMOUNT.

AMOUNT is number in `brb-currency'.

Transaction is recorded into `brb-ledger-file'."
  (when code
    (with-current-buffer (find-file-noselect brb-ledger-file t)
      (revert-buffer t t t)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward (concat "(" code ")") nil 'noerror)
          (forward-line -1)
          (--dotimes 4
            (delete-region (line-beginning-position) (1+ (line-end-position))))))
      (save-buffer)))
  (let* ((cmd (format
               "echo '\n%s%s%s\n    %s  %s %s\n    %s' >> '%s'"
               (format-time-string "%Y/%m/%d" date)
               (if code
                   (concat " (" (s-replace "'" "\\'" code) ")")
                 "")
               (if comment
                   (concat " " (s-replace "'" "\\'" comment))
                 "")
               account-to
               amount
               brb-currency
               account-from
               brb-ledger-file))
         (res (shell-command-to-string cmd)))
    (message res)))

;;;###autoload
(defun brb-ledger-deposit ()
  "Deposit an amount for convive."
  (interactive)
  (let* ((name (seq-find
                (lambda (str)
                  (and (not (s-matches-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
                       (not (s-suffix-p brb-currency str))))
                (s-split
                 "  "
                 (s-chop-prefix "- " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 t)))
         (convive (vulpea-select-from
                   "People"
                   (vulpea-db-query-by-tags-some '("people"))
                   :require-match t
                   :initial-prompt name))
         (data (brb-ledger-data-read))
         (balance (assoc-default (vulpea-note-id convive) (brb-ledger-data-balances data)))
         (amount (read-number "Amount: " (when balance (* -1 balance))))
         (date (org-read-date nil t)))
    (brb-ledger-record-txn
     :date date
     :comment "deposit"
     :account-to (concat "balance:" (vulpea-note-id convive))
     :account-from (concat "convive:" (vulpea-note-id convive))
     :amount amount)
    (brb-ledger-buffer-create)))

;;;###autoload
(cl-defun brb-ledger-charge (&key convive amount date code)
  "Charge an amount from convive.

CONVIVE, AMOUNT and DATE are optional arguments. Unless
specified, user is asked to provide them interactively.

CODE can be passed only in non-interactive usage. See
`brb-ledger-record-txn' to learn about this argument."
  (interactive)
  (let* ((name (unless convive
                 (seq-find
                  (lambda (str)
                    (and (not (s-matches-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
                         (not (s-suffix-p brb-currency str))))
                  (s-split
                   "  "
                   (s-chop-prefix "- " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                   t))))
         (convive (or convive
                      (vulpea-select-from
                       "People"
                       (vulpea-db-query-by-tags-some '("people"))
                       :require-match t
                       :initial-prompt name)))
         (amount (or amount (read-number "Amount: ")))
         (date (or date (org-read-date nil t))))
    (brb-ledger-record-txn
     :date date
     :code code
     :comment "charge"
     :account-to "balance:assets"
     :account-from (concat "balance:" (vulpea-note-id convive))
     :amount amount)
    (when (get-buffer brb-ledger-buffer-name)
      (brb-ledger-buffer-create))))

;;;###autoload
(cl-defun brb-ledger-spend (&key amount date comment code)
  "Spend an amount on event.

AMOUNT, DATE and COMMENT are optional arguments. Unless
specified, user is asked to provide them interactively.

CODE can be passed only in non-interactive usage. See
`brb-ledger-record-txn' to learn about this argument."
  (interactive)
  (let ((amount (or amount (read-number "Amount: ")))
        (date (or date (org-read-date nil t)))
        (comment (or comment (read-string "Comment: "))))
    (brb-ledger-record-txn
     :date date
     :code code
     :comment comment
     :account-to "expenses"
     :account-from "balance:assets"
     :amount amount)
    (when (get-buffer brb-ledger-buffer-name)
      (brb-ledger-buffer-create))))



(cl-defstruct brb-ledger-data
  total
  convives
  balances
  postings)

(cl-defstruct brb-ledger-personal-data total postings)

(cl-defstruct brb-ledger-posting
  date
  description
  account
  amount
  total)

(defun brb-ledger-data-read ()
  "Read balance data from `brb-ledger-file'."
  (let* ((prefix "balance:")
         (ignored '("assets"))

         (cmd-bal (format "hledger -f '%s' balance '%s'" brb-ledger-file prefix))
         (res-bal (split-string (shell-command-to-string cmd-bal) "\n" t " +"))
         (balances (->> (-drop-last 2 res-bal)
                        (--map
                         (let ((vs (split-string it prefix t " +")))
                           (cons
                            (cadr vs)
                            (string-to-number (car vs)))))
                        (--remove (seq-contains-p ignored (car it)))))
         (total (string-to-number (-last-item res-bal)))

         (cmd-accs (format "hledger -f '%s' accounts '%s'" brb-ledger-file prefix))
         (res-accs (shell-command-to-string cmd-accs))
         (convives (emacsql-with-transaction (org-roam-db)
                     (seq-map
                      (lambda (id)
                        (if-let ((convive (vulpea-db-get-by-id id)))
                            convive
                          (user-error "Could not find convive with id %s" id)))
                      (seq-remove
                       (lambda (x)
                         (seq-contains-p ignored x))
                       (seq-map
                        (lambda (x)
                          (string-remove-prefix prefix x))
                        (split-string res-accs "\n" t))))))

         (cmd-register (format "hledger -f '%s' register -O csv -H '%s'"
                               brb-ledger-file prefix))
         (res-register (shell-command-to-string cmd-register))
         (postings (seq-map
                    (lambda (line)
                      (let* ((parts
                              (split-string-and-unquote line ","))
                             (account (string-remove-prefix prefix (nth 4 parts)))
                             (account (or (vulpea-db-get-by-id account)
                                          account)))
                        (make-brb-ledger-posting
                         :date (nth 1 parts)
                         :description (nth 3 parts)
                         :account account
                         :amount (string-to-number (nth 5 parts))
                         :total (string-to-number (nth 6 parts)))))
                    (cdr (split-string res-register "\n" t)))))
    (make-brb-ledger-data
     :total total
     :convives convives
     :balances balances
     :postings postings)))

(defun brb-ledger-personal-data-read ()
  "Read personal balance data from `brb-ledger-file'."
  (let* ((prefix "personal:")

         (cmd-bal (format "hledger -f '%s' balance '%s'" brb-ledger-file prefix))
         (res-bal (split-string (shell-command-to-string cmd-bal) "\n" t " +"))
         (total (string-to-number (-last-item res-bal)))

         (cmd-register (format "hledger -f '%s' register -O csv -H '%s'"
                               brb-ledger-file prefix))
         (res-register (shell-command-to-string cmd-register))
         (postings (seq-map
                    (lambda (line)
                      (let* ((parts (split-string-and-unquote line ","))
                             (account (string-remove-prefix prefix (nth 4 parts)))
                             (account (or (when (string-match org-link-bracket-re account)
                                            (vulpea-db-get-by-id account))
                                          account))
                             (description (->> (nth 3 parts)
                                               (s-chop-prefix "[")
                                               (s-chop-suffix "]")))
                             (description (or (when (string-match string-uuid-regexp description)
                                                (vulpea-db-get-by-id description))
                                              description)))
                        (make-brb-ledger-posting
                         :date (nth 1 parts)
                         :description description
                         :account account
                         :amount (string-to-number (nth 5 parts))
                         :total (string-to-number (nth 6 parts)))))
                    (cdr (split-string res-register "\n" t)))))
    (make-brb-ledger-personal-data
     :total total
     :postings postings)))

(defun brb-ledger-buffer-create ()
  "Create ledger BUFFER and fill it with relevant information.

Return generated buffer."
  (let ((data (brb-ledger-data-read))
        (data-personal (brb-ledger-personal-data-read))
        (buffer (or (get-buffer brb-ledger-buffer-name)
                    (buffer-generate brb-ledger-buffer-name 'unique))))
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
                 (-concat
                  (list
                   (list "Barberry Garden"
                         (brb-ledger--format-amount (brb-ledger-data-total data))
                         (let* ((total (brb-ledger-data-total data))
                                (uncleared (--reduce-from
                                            (+ acc
                                               (or (assoc-default (vulpea-note-id it) (brb-ledger-data-balances data)) 0))
                                            0
                                            (brb-ledger-data-convives data)))
                                (cleared (- total uncleared)))
                           (concat "(" (brb-ledger--format-amount cleared) ")")))
                   (list "Personal"
                         (brb-ledger--format-amount (brb-ledger-personal-data-total data-personal))
                         (concat
                          "("
                          (brb-ledger--format-amount
                           (+
                            (brb-ledger-data-total data)
                            (brb-ledger-personal-data-total data-personal)))
                          ")")))
                  (->> (brb-ledger-data-convives data)
                       (--sort
                        (< (or (assoc-default (vulpea-note-id it) (brb-ledger-data-balances data)) 0)
                           (or (assoc-default (vulpea-note-id other) (brb-ledger-data-balances data)) 0)))
                       (--remove (= 0 (or (assoc-default (vulpea-note-id it) (brb-ledger-data-balances data)) 0)))
                       (--map
                        (list
                         (vulpea-note-title it)
                         (brb-ledger--format-amount
                          (or (assoc-default (vulpea-note-id it)
                                             (brb-ledger-data-balances data))
                              0)
                          :positive-face 'warning
                          :zero-face 'success)
                         ""))))
                 :pad-type '(right left left)
                 :row-start "- "
                 :sep "  ")
                ""
                (propertize "Latest transactions: Barberry Garden" 'face 'bold)
                ""
                (string-table
                 :data (->> (brb-ledger-data-postings data)
                            (--remove (string-equal "charge" (brb-ledger-posting-description it)))
                            (seq-reverse)
                            (-take 36)
                            (--map (list
                                    (propertize (brb-ledger-posting-date it) 'face 'shadow)
                                    (cond
                                     ((vulpea-note-p (brb-ledger-posting-account it))
                                      (vulpea-note-title (brb-ledger-posting-account it)))

                                     ((vulpea-note-p (brb-ledger-posting-description it))
                                      (vulpea-note-title (brb-ledger-posting-description it)))

                                     (t (brb-ledger-posting-description it)))
                                    (brb-ledger--format-amount (brb-ledger-posting-amount it))
                                    "->"
                                    (brb-ledger--format-amount (brb-ledger-posting-total it)))))
                 :width '(nil 70 nil nil nil)
                 :pad-type '(left right left left left)
                 :row-start "- "
                 :sep "  ")
                ""
                (propertize "Latest transactions: personal" 'face 'bold)
                ""
                (string-table
                 :data (->> (brb-ledger-personal-data-postings data-personal)
                            (seq-reverse)
                            (-take 36)
                            (--map (list
                                    (propertize (brb-ledger-posting-date it) 'face 'shadow)
                                    (if (vulpea-note-p (brb-ledger-posting-account it))
                                        (vulpea-note-title (brb-ledger-posting-account it))
                                      (brb-ledger-posting-description it))
                                    (brb-ledger--format-amount (brb-ledger-posting-amount it))
                                    "->"
                                    (brb-ledger--format-amount (brb-ledger-posting-total it)))))
                 :width '(nil 70 nil nil nil)
                 :pad-type '(left right left left left)
                 :row-start "- "
                 :sep "  "))
          "\n")))
      (result-present-mode +1))
    buffer))

;;;###autoload
(defun brb-ledger-display ()
  "Display Barberry Garden ledger."
  (interactive)
  (let ((buffer (brb-ledger-buffer-create)))
    (switch-to-buffer buffer)))

(cl-defun brb-ledger--format-amount (amount
                                     &key
                                     positive-face
                                     zero-face
                                     negative-face)
  "Format balance represented as AMOUNT.

Uses POSITIVE-FACE, ZERO-FACE and NEGATIVE-FACE for prettifying."
  (let* ((value (brb-price-format amount))
         (face (cond
                ((> amount 0)
                 (or positive-face 'success))
                ((< amount 0)
                 (or negative-face 'warning))
                (t
                 (or zero-face 'warning)))))
    (propertize value 'face face)))



(cl-defun brb-ledger-balance-of (convive &optional date)
  "Return balance of a given CONVIVE.

Optionally return the balance on DATE (inclusive).

Result is a number in `brb-currency'."
  (let* ((id (if (vulpea-note-p convive) (vulpea-note-id convive) convive))
         (cmds (if date
                   (let* ((time0 (if (stringp date) (date-to-time date) date))
                          (time1 (time-add time0 (* 60 60 24))))
                     (list
                      (format "hledger -f %s balance balance:%s -e %s"
                              brb-ledger-file
                              id
                              (format-time-string "%Y-%m-%d" time0))
                      (format "hledger -f %s balance balance:%s -b %s -e %s 'not:desc:charge'"
                              brb-ledger-file
                              id
                              (format-time-string "%Y-%m-%d" time0)
                              (format-time-string "%Y-%m-%d" time1))))
                 (list
                  (format "hledger -f %s balance balance:%s" brb-ledger-file id)))))
    (->> cmds
         (--map
          (->> it
               (shell-command-to-string)
               (s-trim)
               (s-lines)
               (-last-item)
               (string-to-number)))
         (--reduce-from (+ acc it) 0))))



(cl-defun brb-ledger-buy-wine (&key wine price date code)
  "Buy a WINE from personal account.

PRICE is the actual purchase price. It must be in `brb-currency'.

DATE is purchase date (internal time).

See `brb-ledger-record-txn' to learn about CODE."
  (let* ((wine-id (if (vulpea-note-p wine)
                      (vulpea-note-id wine)
                    wine)))
    (brb-ledger-record-txn
     :date date
     :code code
     :comment (concat "[" wine-id "]")
     :account-to "spending:wines"
     :account-from "personal:account"
     :amount price)))



(cl-defun brb-ledger-buy-wines-for (&key convive
                                         spend-amount
                                         charge-amount
                                         date)
  "Buy wines for CONVIVE.

Spend SPEND-AMOUNT on DATE and charge CHARGE-AMOUNT said CONVIVE.

Basically a convenient shortcut for charge + spend."
  (interactive)
  (let* ((name (unless convive
                 (seq-find
                  (lambda (str)
                    (and (not (s-matches-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str))
                         (not (s-suffix-p brb-currency str))))
                  (s-split
                   "  "
                   (s-chop-prefix "- " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                   t))))
         (convive (or convive
                      (vulpea-select-from
                       "People"
                       (vulpea-db-query-by-tags-some '("people"))
                       :require-match t
                       :initial-prompt name)))
         (spend-amount (or spend-amount (read-number "Spend amount: ")))
         (charge-amount (or charge-amount (read-number "Charge amount: ")))
         (date (or date (org-read-date nil t))))
    (brb-ledger-spend :amount spend-amount
                      :date date
                      :comment (concat "Wine for " (vulpea-note-title convive)))
    (brb-ledger-charge :convive convive
                       :amount charge-amount
                       :date date)))

(cl-defun brb-ledger-receive-present (&optional source amount date)
  "Receive AMOUNT as a present from SOURCE on a DATE."
  (interactive)
  (let* ((source (or source
                     (vulpea-select "Source" :require-match t)))
         (amount (or amount (read-number "Amount: ")))
         (date (or date (org-read-date nil t))))
    (brb-ledger-record-txn
     :date date
     :comment "present"
     :account-to "balance:assets"
     :account-from (concat "source:" (vulpea-note-id source))
     :amount amount)
    (brb-ledger-buffer-create)))



(provide 'lib-brb-ledger)
;;; lib-brb-ledger.el ends here
