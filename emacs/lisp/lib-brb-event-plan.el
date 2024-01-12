;;; lib-brb-event-plan.el --- Utilities for event planning -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2023, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Mar 2023
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
;;; Code:

(require 'org-ml)
(require 'vulpea)
(require 'lib-plist)
(require 'lib-vino-stats)
(require 'lib-brb-event)
(require 'lib-brb-ledger)



;;;###autoload
(defun brb-event-plan (&optional event)
  "Start planning session for barberry garden EVENT."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (data (brb-event-plan--data-read event))
         (buffer (buffer-generate (format "*%s*" (vulpea-note-title event)) 'unique)))
    (brb-event-plan--propagate buffer event data (make-hash-table :test 'equal))
    (pop-to-buffer buffer)))



(defun brb-event-plan--propagate (buffer event data balances)
  "Propagate planning BUFFER for EVENT.

DATA is a properly list containing information about shared and
personal spendings.

BALANCES is a hash-table, where key is participant id and value
is balance."
  (let* ((wines (brb-event-wines event))
         (date (vulpea-utils-with-note event
                 (vulpea-buffer-prop-get "date")))
         (charge-narrator (plist-get data :charge-narrator))
         (participants-all (brb-event-participants event))
         (participants (--remove (and (not charge-narrator)
                                      (string-equal (vulpea-note-id it) brb-event-narrator-id))
                                 participants-all))
         (planned-participants (or (plist-get data :planned-participants) 0))
         (planned-spending-shared (or (plist-get data :planned-shared-spending) 0))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (wine-prices (brb-event-wines--prices event))
         (invoice (brb-event-plan--invoice event data participants wine-prices))

         (wines-total-public (-sum (plist-get wine-prices :public)))

         (balances (or balances (make-hash-table :test 'equal)))
         (point))
    (--each participants
      (let ((id (vulpea-note-id it)))
        (unless (gethash id balances)
          (puthash id (brb-ledger-balance-of id date) balances))))
    (with-current-buffer buffer
      (setq point (point))
      (erase-buffer)
      (insert
       (propertize
        (let ((s (concat "=== " (vulpea-buttonize event) " ===")))
          (concat
           (s-repeat (length s) "_") "\n"
           s "\n"
           (s-repeat (length s) "‾")))
        'face 'help-for-help-header)
       "\n"

       (buttonize "[Refresh]"
                  (lambda (&rest _)
                    (brb-event-plan--propagate
                     buffer (vulpea-db-get-by-id (vulpea-note-id event)) (brb-event-plan--data-read event) balances)))
       " "
       (buttonize "[TG Report]" #'brb-event-plan-display-tg-report event)
       " "
       (buttonize "[Record spendings]"
                  (lambda (&rest _)
                    (brb-ledger-record-txn
                     :amount (plist-get invoice :wines-total)
                     :date (date-to-time date)
                     :comment (format "%s: wines" (vulpea-note-title event))
                     :code (concat (vulpea-note-id event) ":wines")
                     :account-to "personal:account"
                     :account-from "balance:assets")
                    (brb-ledger-spend
                     :amount (plist-get invoice :shared-total)
                     :date (date-to-time date)
                     :comment (format "%s: shared" (vulpea-note-title event))
                     :code (concat (vulpea-note-id event) ":shared"))
                    (brb-ledger-spend
                     :amount (plist-get invoice :personal-total)
                     :date (date-to-time date)
                     :comment (format "%s: delivery" (vulpea-note-title event))
                     :code (concat (vulpea-note-id event) ":delivery"))
                    (message "Done.")))
       "\n\n")

      (insert
       (propertize "1. Planning" 'face 'org-level-1) "\n\n"
       (string-table
        :row-start "- "
        :pad-type '(right left left)
        :sep "  "
        :data
        (let* ((total-public (+ wines-total-public planned-spending-shared))
               (total-real (+ (plist-get invoice :wines-total) planned-spending-shared))
               (price-public (ceiling (if (> planned-participants 0) (/ total-public planned-participants) 0)))
               (price-real (ceiling (if (> planned-participants 0) (/ total-real planned-participants) 0)))
               (debit (* planned-participants price))
               (gain-public (- debit total-public))
               (gain-real (- debit total-real)))
          `(("Planned participants:"
             ,(plist-buttonize-prop data :planned-participants 0
               (lambda (data)
                 (brb-event-plan--data-write event data)
                 (brb-event-plan--propagate buffer event data balances)))
             "")
            ("Price:" ,(vulpea-meta-buttonize event "price" 'number
                        (lambda (event) (brb-event-plan--propagate buffer event data balances))
                        :default 0
                        :to-string #'brb-price-format)
             "")
            ("Spending (shared):"
             ,(plist-buttonize-prop data :planned-shared-spending 0
               (lambda (data)
                 (brb-event-plan--data-write event data)
                 (brb-event-plan--propagate buffer event data balances))
               #'brb-price-format)
             "")
            ("Spending (wines):"
             ,(brb-price-format wines-total-public)
             ,(brb-price-format (plist-get invoice :wines-total)))
            ("Spending (total):" ,(brb-price-format total-public) ,(brb-price-format total-real))
            ("Price recommended (0%):" ,(brb-price-format price-public) ,(brb-price-format price-real))
            ("Price recommended (10%):"
             ,(brb-price-format (ceiling (* price-public 1.1)))
             ,(brb-price-format (ceiling (* price-real 1.1))))
            ("Price recommended (25%):"
             ,(brb-price-format (ceiling (* price-public 1.25)))
             ,(brb-price-format (ceiling (* price-real 1.25))))
            ("Price recommended (33%):"
             ,(brb-price-format (ceiling (* price-public 1.33)))
             ,(brb-price-format (ceiling (* price-real 1.33))))
            ("Planned debit:" ,(brb-price-format debit) "")
            ("Planned gain:"
             ,(propertize (brb-price-format gain-public) 'face (if (>= gain-public 0) 'success 'error))
             ,(propertize (brb-price-format gain-real) 'face (if (>= gain-real 0) 'success 'error))))))
       "\n\n")

      (insert
       (propertize "2. Reality" 'face 'org-level-1) "\n\n"
       (string-table
        :pad-type '(right left)
        :sep "    "
        :data
        `(("Charge narrator:"
           ,(plist-buttonize-prop data :charge-narrator nil
             (lambda (data)
               (brb-event-plan--data-write event data)
               (brb-event-plan--propagate buffer event data balances))
             (lambda (v) (if v "[on]" "[off]"))
             (lambda (_) (not charge-narrator))))))
       "\n\n"
       (string-table
        :row-start "- "
        :pad-type '(right left left)
        :sep "  "
        :data
        `(("Participants:" ,(number-to-string (seq-length participants)))
          ("Price:" ,(vulpea-meta-buttonize event "price" 'number
                      (lambda (event) (brb-event-plan--propagate buffer event data balances))
                      :default 0
                      :to-string #'brb-price-format))
          ("Spending (shared):" ,(brb-price-format (plist-get invoice :shared-total)))
          ("Spending (wines):" ,(brb-price-format (plist-get invoice :wines-total)))
          ("Spending (total):" ,(brb-price-format (plist-get invoice :credit)))
          ("Gain:" ,(let ((gain (plist-get invoice :balance)))
                     (propertize (brb-price-format gain) 'face (if (>= gain 0) 'success 'error))))))
       "\n\n")

      (insert
       (propertize
        (concat
         "2.1 Participants "
         (buttonize "[+]" (lambda (&rest _)
                            (let ((participant (vulpea-select-from "Participant"
                                                                   (--remove
                                                                    (-contains-p (-map #'vulpea-note-id participants-all) (vulpea-note-id it))
                                                                    (vulpea-db-query-by-tags-every '("people")))
                                                                   :require-match t)))
                              (vulpea-utils-with-note event
                                (vulpea-buffer-meta-set "participants" (cons participant participants-all) 'append)
                                (save-buffer))
                              (brb-event-plan--propagate
                               buffer (vulpea-db-get-by-id (vulpea-note-id event)) (brb-event-plan--data-read event) balances)))))
        'face 'org-level-2)
       "\n\n")
      (--each participants
        (insert
         (buttonize "[x]" (lambda (&rest _)
                             (vulpea-utils-with-note event
                               (vulpea-buffer-meta-set "participants" (-remove (lambda (other)
                                                                                 (string-equal (vulpea-note-id it) (vulpea-note-id other)))
                                                                               participants-all)
                                                       'append)
                               (save-buffer))
                             (brb-event-plan--propagate
                               buffer (vulpea-db-get-by-id (vulpea-note-id event)) (brb-event-plan--data-read event) balances)))
         " "
         (vulpea-buttonize it)
         "\n"))
      (insert "\n")

      (insert
       (propertize "3. Wine" 'face 'org-level-1) "\n\n"
       (string-table
        :header '("country" "producer" "name" "vintage" "public P" "real P")
        :pad-type '(right right right left left left)
        :width '(nil nil 32 nil nil nil nil)
        :header-sep "-"
        :header-sep-start "|-"
        :header-sep-conj "-+-"
        :header-sep-end "-|"
        :row-start "| "
        :row-end " |"
        :sep " | "
        :data
        (--> wines
             (--map-indexed
              (let* ((roa (or (vulpea-note-meta-get it "region" 'note)
                              (vulpea-note-meta-get it "appellation" 'note)))
                     (country (vulpea-note-meta-get roa "country" 'note)))
                (list country
                      (vulpea-note-meta-get it "producer" 'note)
                      (vulpea-note-meta-get it "name")
                      (or (vulpea-note-meta-get it "vintage") "NV")
                      (brb-price-format (or (nth it-index (plist-get wine-prices :public)) 0))
                      (brb-price-format (or (nth it-index (plist-get wine-prices :real)) 0))))
              it)
             (-concat it
                      '(sep)
                      `((""
                         ""
                         ,(seq-length wines)
                         ""
                         ,(brb-price-format wines-total-public)
                         ,(brb-price-format (plist-get invoice :wines-total)))))))
       "\n\n")

      (insert (propertize "4. Spending" 'face 'org-level-1) "\n\n")

      (insert
       (propertize
        (concat "4.1. Shared "
                (buttonize "[+]"
                           (lambda (&rest _)
                             (let ((item (read-string "Item: "))
                                   (price (read-number "Price: "))
                                   (amount (read-number "Amount: ")))
                               (--> (plist-get data :shared)
                                    (-snoc it `(:item ,item :amount ,amount :price ,price))
                                    (plist-put data :shared it)
                                    (setq data it))
                               (brb-event-plan--data-write event data)
                               (brb-event-plan--propagate buffer event data balances)))))
        'face 'org-level-2)
       "\n\n"
       (string-table
        :header '("" "item" "price" "amount" "total")
        :pad-type '(left right left left left)
        :header-sep "-"
        :header-sep-start "|-"
        :header-sep-conj "-+-"
        :header-sep-end "-|"
        :row-start "| "
        :row-end " |"
        :sep " | "
        :data
        (--> (plist-get data :shared)
             (--map
              (list
               (buttonize "[x]"
                          (lambda (&rest _)
                            (->> (plist-get data :shared)
                                 (-remove (lambda (item)
                                            (string-equal (plist-get item :item)
                                                          (plist-get it :item))))
                                 (plist-put data :shared)
                                 (setq data))
                            (brb-event-plan--data-write event data)
                            (brb-event-plan--propagate buffer event data balances)))
               (plist-get it :item)
               (brb-price-format (plist-get it :price))
               (plist-get it :amount)
               (brb-price-format (ceiling
                                  (* (plist-get it :amount)
                                     (plist-get it :price)))))
              it)
             (-concat
              it
              '(sep)
              `(("" "" "" "" ,(brb-price-format (plist-get invoice :shared-total)))))))
       "\n\n")

      ;; '(:personal ((:item "item 1"
      ;;               :price 100
      ;;               :orders (:participant "id-1" :amount 1)
      ;;                       (:participant "id-2" :amount 2))))
      (insert
       (propertize
        (concat "4.2. Delivery "
                (buttonize "[+]"
                           (lambda (&rest _)
                             (let* ((data (or data '(:personal nil)))
                                    (participant (vulpea-select-from "Person" participants-all :require-match t))
                                    (id (vulpea-note-id participant))
                                    (personal (plist-get data :personal))
                                    (name (completing-read "Item: " (->> personal (--map (plist-get it :item)))))
                                    (price (or (plist-get (--find (string-equal name (plist-get it :item)) personal)
                                                          :price)
                                               (read-number "Price: ")))
                                    (amount (read-number "Amount: ")))
                               ;; make sure given item exists
                               (unless (--find (string-equal (plist-get it :item) name) personal)
                                 (cl-pushnew (list :item name :price price :orders nil) personal))

                               ;; make sure order for given participant exists
                               (unless (--> (--find (string-equal (plist-get it :item) name) personal)
                                            (plist-get it :orders)
                                            (--find (string-equal (plist-get it :participant) id) it))
                                 (cl-pushnew
                                  (list :participant id :amount 0)
                                  (--> (--find (string-equal (plist-get it :item) name) personal)
                                       (plist-get it :orders))))

                               ;; update data
                               (cl-incf (--> (--find (string-equal (plist-get it :item) name) personal)
                                             (plist-get it :orders)
                                             (--find (string-equal (plist-get it :participant) id ) it)
                                             (plist-get it :amount))
                                        amount)
                               (plist-put data :personal personal)
                               (brb-event-plan--data-write event data)
                               (brb-event-plan--propagate buffer event data balances)))))
        'face 'org-level-2)
       "\n\n"
       (string-table
        :header '("item" "price" "q" "total" "participants")
        :pad-type '(right right left left right)
        :width '(24 nil nil nil 42)
        :header-sep "-"
        :header-sep-start "|-"
        :header-sep-conj "-+-"
        :header-sep-end "-|"
        :row-start "| "
        :row-end " |"
        :sep " | "
        :data
        (let ((delivery-amount (->> (plist-get data :personal)
                                    (--map (plist-get it :orders))
                                    (-flatten-n 1)
                                    (--map (plist-get it :amount))
                                    (-sum)))
              (delivery-total (->> (plist-get data :personal)
                                   (--map (ceiling (* (->> (plist-get it :orders)
                                                           (--map (plist-get it :amount))
                                                           (-sum))
                                                      (plist-get it :price))))
                                   (-sum))))
          (--> (plist-get data :personal)
               (--map
                (let* ((amount (->> (plist-get it :orders)
                                    (--map (plist-get it :amount))
                                    (-sum)))
                       (total (ceiling (* amount (plist-get it :price)))))
                  (list
                   (plist-get it :item)
                   (brb-price-format (plist-get it :price))
                   amount
                   (brb-price-format total)
                   (->> (plist-get it :orders)
                        (--map (plist-get it :participant))
                        (--map (-find
                                (lambda (p)
                                  (string-equal it (vulpea-note-id p)))
                                participants-all))
                        (-map #'vulpea-buttonize)
                        (s-join ", "))))
                it)
               (-concat
                it
                '(sep)
                `(("" "" ,delivery-amount ,delivery-total ""))))))
       "\n\n")

      (insert
       (propertize "5. Personal" 'face 'org-level-1) "\n\n"
       (buttonize "[Charge everyone]"
                  (lambda (&rest _) (error "Not implemented")))
       "\n\n")
      (--each-indexed participants
        ;; '(:personal ((:item "item 1"
        ;;               :price 100
        ;;               :orders (:participant "id-1" :amount 1)
        ;;                       (:participant "id-2" :amount 2))))
        (let* ((balance (or (gethash (vulpea-note-id it) balances) 0))
               (invoice-personal (brb-event-plan--personal-invoice it balance data price)))
          (insert
           (propertize (format "4.%0d. %s" (1+ it-index) (vulpea-note-title it)) 'face 'org-level-2)
           "\n\n"
           (buttonize "[Charge]"
                      (lambda (&rest _)
                        (brb-ledger-charge
                         :convive it
                         :code (concat (vulpea-note-id event) ":" (vulpea-note-id it))
                         :amount (plist-get invoice-personal :charge)
                         :date (date-to-time date))))
           " "
           (buttonize "[Statement]"
                      (lambda (&rest _)
                        (brb-event-plan-display-statement event it invoice-personal)))
           "\n\n"
           (brb-event-plan--personal-invoice-to-string invoice-personal)
           "\n\n")))
      (ws-butler-clean-region (point-min) (point-max))
      (goto-char point)
      (ignore-errors
        (recenter)))))



(defun brb-event-plan-invoice (event)
  "Prepare EVENT invoice."
  (let ((charge-narrator (vulpea-note-meta-get event "charge narrator" 'symbol)))
    (brb-event-plan--invoice
     event (brb-event-plan--data-read event)
     (--remove (and (not charge-narrator)
                    (string-equal (vulpea-note-id it) brb-event-narrator-id))
               (brb-event-participants event))
     (brb-event-wines--prices event))))

(defun brb-event-plan--invoice (event data participants wine-prices)
  "Prepare EVENT invoice.

Invoice calculations are based on plan DATA, PARTICIPANTS and
WINE-PRICES."
  (let* ((price (or (vulpea-note-meta-get event "price" 'number) 0))
         (shared-total (->> (plist-get data :shared)
                            (--map (ceiling
                                    (* (plist-get it :amount)
                                       (plist-get it :price))))
                            (-sum)))
         (wines-total (-sum (plist-get wine-prices :real)))
         (personal-total (->> (plist-get data :personal)
                              (--map (ceiling (* (->> (plist-get it :orders)
                                                      (--map (plist-get it :amount))
                                                      (-sum))
                                                 (plist-get it :price))))
                              (-sum)))
         (participants-len (seq-length participants))
         (debit (* participants-len price))
         (credit (+ wines-total shared-total))
         (balance (- debit credit)))
    (list
     :shared-total shared-total
     :wines-total wines-total
     :personal-total personal-total
     :debit debit
     :credit credit
     :balance balance)))



(defun brb-event-plan--data-file (event)
  "Return path to data file of EVENT."
  (file-name-with-extension (vulpea-note-path event) "plan.el"))

(defun brb-event-plan--data-read (event)
  "Read `brb-event-plan--data' for EVENT."
  (let ((file (brb-event-plan--data-file event)))
    (when (file-exists-p file)
      (with-temp-buffer
        (condition-case nil
	          (progn
	            (insert-file-contents file)
              (read (current-buffer)))
	        (error
	         (message "Could not read data from %s" file)))))))

(defun brb-event-plan--data-write (event data)
  "Write DATA for EVENT."
  (let ((file (brb-event-plan--data-file event)))
    (with-temp-file file
      (let ((print-level nil)
	          (print-length nil))
	      (print data (current-buffer))))))



(defun brb-event-plan-display-tg-report (&optional event)
  "Display TG report for EVENT."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (buffer (get-buffer-create (format "*%s TG report*" (vulpea-note-title event))))
         (summary (assoc-default 'wines (brb-event-summary event))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert
       (->> (brb-event-wines event)
            (--map-indexed
             (format "☆ %s - [%s](https://barberry.io/wines/%s.html)"
                     (if-let ((wavg (assoc-default 'wavg (nth it-index summary))))
                         (format "%.2f" wavg) "N/A ")
                     (vulpea-note-title it)
                     (vulpea-note-id it)))
            (s-join "\n"))))
    (pop-to-buffer buffer)))



(defun brb-event-plan--personal-invoice (participant balance data price)
  "Prepare invoice for PARTICIPANT.

Invoice is based on event PRICE, event DATA and current BALANCE."
  (let* ((id (vulpea-note-id participant))
         (order (->> (plist-get data :personal)
                     (--filter (-any-p (lambda (order) (string-equal id (plist-get order :participant)))
                                       (plist-get it :orders)))
                     (--map (list :item (plist-get it :item)
                                  :price (plist-get it :price)
                                  :amount (plist-get
                                           (--find (string-equal id (plist-get it :participant))
                                                   (plist-get it :orders))
                                           :amount)))))
         (total (- (+ price (->> order
                                 (--map (ceiling (* (plist-get it :amount)
                                                    (plist-get it :price))))
                                 (-sum)))
                   balance)))
    (list
     :price price
     :balance balance
     :order order
     :total total
     :charge (+ balance total))))

(defun brb-event-plan--personal-invoice-to-string (invoice)
  "Format personal INVOICE."
  (concat
   "- Balance: " (brb-price-format (plist-get invoice :balance)) "\n"
   "- Event: " (brb-price-format (plist-get invoice :price)) "\n"
   (mapconcat
    (lambda (it)
      (format "- %s (x%.2f): %s\n"
              (plist-get it :item)
              (plist-get it :amount)
              (brb-price-format
               (ceiling (* (plist-get it :amount)
                           (plist-get it :price))))))
    (plist-get invoice :order))
   "- Total: " (brb-price-format (plist-get invoice :total))))

(defun brb-event-plan-display-statement (event participant invoice)
  "Display an INVOICE for PARTICIPANT of EVENT."
  (let* ((narrator (vulpea-db-get-by-id brb-event-narrator-id))
         (buffer (get-buffer-create (format "*statement for %s*" (vulpea-note-title participant)))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert
       "👋 Thank you for participating in " (vulpea-note-title event) "!\n\n"
       "📋 You can find more information about tasted wines and winners on Barberry Garden - "
       (format "https://barberry.io/posts/%s-%s.html"
               (vulpea-utils-with-note event
                 (format-time-string "%Y-%m-%d" (date-to-time (vulpea-buffer-prop-get "date"))))
               (vulpea-utils-with-note event
                 (vulpea-buffer-prop-get "slug")))
       ".\n\n"
       "🧾 This is your receipt:\n\n"
       (brb-event-plan--personal-invoice-to-string invoice)
       "\n\n"
       (if (> (plist-get invoice :total) 0)
           (concat
            "mono:   " (vulpea-note-meta-get narrator "cc mono") "\n"
            "ukrsib: " (vulpea-note-meta-get narrator "cc ukrsib") "\n"
            "web:    " (vulpea-note-meta-get narrator "send mono") "\n"
            "\n")
         "")
       "🥂 Cheers! See you next time!"))
    (pop-to-buffer buffer)))



(provide 'lib-brb-event-plan)
;;; lib-brb-event-plan.el ends here
