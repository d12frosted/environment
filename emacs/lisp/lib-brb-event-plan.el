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

(require 'vulpea)
(require 'lib-plist)
(require 'lib-vino-stats)
(require 'lib-brb-event)
(require 'lib-brb-ledger)
(require 'lib-hash-table)

;; * Entry point

;;;###autoload
(defun brb-event-plan (&optional event)
  "Start planning session for barberry garden EVENT."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (buffer (buffer-generate (format "*%s*" (vulpea-note-title event)) 'unique))
         (x (-> (ep--create :buffer buffer
                            :data (brb-event-data-read event)
                            :tab 'plan)
                (ep-set-event event)
                (ep-set-balances))))
    (brb-event-plan--propagate-new x)
    (switch-to-buffer buffer)))

;; * Event Plan

(cl-defstruct (ep (:constructor ep--create)
                  (:copier nil))
  buffer
  data
  tab
  event
  host
  participants
  waiting
  wines
  balances)

(cl-defun ep-set-event (x event)
  "Set EVENT slot in X."
  (let* ((host (vulpea-note-meta-get event "host" 'note))
         (participants (brb-event-participants event))
         (waiting (vulpea-note-meta-get-list event "waiting" 'note))
         (wines (brb-event-wines event)))
    (setf (ep-event x) event
          (ep-host x) host
          (ep-participants x) participants
          (ep-waiting x) waiting
          (ep-wines x) wines)
    x))

(cl-defun ep-set-balances (x)
  "Set balances slot in X."
  (let* ((date (vulpea-utils-with-note (ep-event x)
                 (vulpea-buffer-prop-get "date")))
         (balances (-> (--map `((pid . ,(vulpea-note-id it))
                                (balance . ,(brb-ledger-balance-of (vulpea-note-id it) date)))
                              (ep-participants x))
                       (hash-table-from :key-fn (-partial #'alist-get 'pid)
                                        :value-fn (-partial #'alist-get 'balance)))))
    (setf (ep-balances x) balances)
    x))

;; ** Refresh

(cl-defmethod ep-refresh ((x ep))
  "Refresh X."
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-reload-event ((x ep))
  "Reload event of X."
  (let ((event (vulpea-db-get-by-id (vulpea-note-id (ep-event x)))))
    (brb-event-plan--propagate-new
     (-> (ep--create :buffer (ep-buffer x)
                     :data (brb-event-data-read event)
                     :tab (ep-tab x)
                     :balances (ep-balances x))
         (ep-set-event event)))))

(cl-defmethod ep-reload ((x ep))
  "Reload X."
  (let ((event (vulpea-db-get-by-id (vulpea-note-id (ep-event x)))))
    (brb-event-plan--propagate-new
     (-> (ep--create :buffer (ep-buffer x)
                     :data (brb-event-data-read event)
                     :tab (ep-tab x))
         (ep-set-event event)
         (ep-set-balances)))))

;; ** Saving

(cl-defmethod ep-save-data ((x ep) data)
  "Save DATA and reload X."
  (setf (ep-data x) data)
  (brb-event-data-write (ep-event x) (ep-data x))
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-save-event ((x ep) (event vulpea-note))
  "Save EVENT and reload X."
  (brb-event-plan--propagate-new (ep-set-event x event)))

;; ** Tabs

(cl-defmethod ep-set-tab ((x ep) tab)
  "Set TAB and reload X."
  (setf (ep-tab x) tab)
  (brb-event-plan--propagate-new x))

;; ** Financial calculations

(cl-defmethod ep-statement-for ((x ep) participant)
  "Return financial statement for PARTICIPANT of X."
  (if (->> (ep-data x)
           (alist-get 'participants)
           (--mapcat (alist-get 'pays-for it))
           (-uniq)
           (--find (string-equal it (vulpea-note-id participant))))
      (brb-event-empty-statement-for
       (ep-event x)
       participant
       :data (ep-data x)
       :wines (ep-wines x)
       :balances (ep-balances x))
    (--reduce-from
     (brb-event-statement-add
      acc
      (brb-event-statement-for
       (ep-event x)
       it
       :data (ep-data x)
       :host (ep-host x)
       :wines (ep-wines x)
       :balances (ep-balances x)))
     (brb-event-statement-for
      (ep-event x)
      participant
      :data (ep-data x)
      :host (ep-host x)
      :wines (ep-wines x)
      :balances (ep-balances x))
     (->> (ep-data x)
          (alist-get 'participants)
          (--find (string-equal
                   (vulpea-note-id participant)
                   (alist-get 'id it)))
          (alist-get 'pays-for)
          (-map (lambda (pid)
                  (--find
                   (string-equal pid (vulpea-note-id it))
                   (ep-participants x))))))))

(cl-defmethod ep-statement ((x ep))
  "Return financial statement for X."
  (brb-event-statement
   (ep-event x)
   :data (ep-data x)
   :host (ep-host x)
   :participants (ep-participants x)
   :wines (ep-wines x)
   :balances (ep-balances x)))

(defun ep--glass-price (it)
  "Calculate glass price of an extra wine IT."
  (let* ((price (or (assoc-default 'price-asking it)
                    (assoc-default 'price-public it)))
         (ps (assoc-default 'participants it))
         (glass-price ))
    (if ps (ceiling (/ price (float (length ps)))) 0)))

(cl-defmethod ep-add-pays-for ((x ep) payer participant)
  "Make PAYER to pay for PARTICIPANT.

X is `ep'."
  (let* ((data (ep-data x)))
    (setf (alist-get 'participants data)
          (-update-first-by
           (lambda (pd) (string-equal (vulpea-note-id payer) (alist-get 'id pd)))
           (lambda (pd)
             (setf (alist-get 'pays-for pd)
                   (-uniq (append (alist-get 'pays-for pd) (list (vulpea-note-id participant)))))
             pd)
           `((id . ,(vulpea-note-id payer))
             (pays-for . (,(vulpea-note-id participant))))
           (alist-get 'participants data)))
    (ep-save-data x data)))

(cl-defmethod ep-remove-pays-for ((x ep) payer participant)
  "Stop making PAYER to pay for PARTICIPANT.

X is `ep'."
  (let* ((data (ep-data x)))
    (setf (alist-get 'participants data)
          (-update-first-by
           (lambda (pd) (string-equal (vulpea-note-id payer) (alist-get 'id pd)))
           (lambda (pd)
             (setf (alist-get 'pays-for pd)
                   (->> (alist-get 'pays-for pd)
                        (--remove (string-equal it (vulpea-note-id participant)))
                        (-uniq)))
             pd)
           nil
           (alist-get 'participants data)))
    (ep-save-data x data)))

;; * Wine data manipulations

(defun ep-update-score-data (x wid pid fn def)
  "Update score data in X using FN.

WID is wine id.
PID is participant id.

When score data for given WID and PID not found, use DEF."
  (let* ((data (ep-data x)))
    (setf (alist-get 'wines data)
          (-update-first-by
           (lambda (wd) (string-equal wid (alist-get 'id wd)))
           (lambda (wd)
             (setf (alist-get 'scores wd)
                   (-update-first-by
                    (lambda (sd) (string-equal pid (alist-get 'participant sd)))
                    fn
                    def
                    (alist-get 'scores wd)))
             wd)
           ;; TODO set more meaningful value
           nil
           (alist-get 'wines data)))
    (ep-save-data x data)))

(defun ep-set-score (x wid pid score)
  "Set SCORE for wine in X.

WID is wine id.
PID is participant id."
  (ep-update-score-data x wid pid
                        (lambda (sd)
                          (setf (alist-get 'score sd) score)
                          sd)
                        `((participant . ,pid)
                          (score . ,score)
                          (sentiment . nil))))

(defun ep-set-sentiment (x wid pid sentiment)
  "Set SENTIMENT for wine in X.

WID is wine id.
PID is participant id."
  (ep-update-score-data x wid pid
                        (lambda (sd)
                          (setf (alist-get 'sentiment sd) sentiment)
                          sd)
                        `((participant . ,pid)
                          (score . ,nil)
                          (sentiment . ,sentiment))))

;; * Buffer building blocks

(defun brb-event-plan--propagate-new (x)
  "Propagate planning buffer for X."
  (with-current-buffer (ep-buffer x)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil)
          (point))
      (setq point (point))
      (erase-buffer)

      ;; content
      (brb-event-plan--header x)
      (insert "\n")
      (pcase (ep-tab x)
        (`plan (brb-event-plan--tab-plan x))
        (`scores (brb-event-plan--tab-scores x))
        (`order (brb-event-plan--tab-order x))
        (`extra (brb-event-plan--tab-extra x))
        (`invoices (brb-event-plan--tab-invoices x)))
      (insert "\n")

      (ws-butler-clean-region (point-min) (point-max))
      (goto-char point))))

(cl-defmethod brb-event-plan--header ((x ep))
  "Insert header for X."
  (let ((tabs '(plan scores order extra invoices))
        (set-tab (-partial #'ep-set-tab x)))
    (insert
     (propertize
      (let ((s (concat "=== " (vulpea-buttonize (ep-event x)) " ===")))
        (concat
         (s-repeat (length s) "_") "\n"
         s "\n"
         (s-repeat (length s) "‾")))
      'face 'help-for-help-header)
     "\n"
     (mapconcat
      (lambda (tab)
        (if (eq (ep-tab x) tab)
            (propertize
             (buttonize (concat "[*" (symbol-name tab) "*]") set-tab tab)
             'face 'barberry-theme-face-strong)
          (buttonize (concat "[" (symbol-name tab) "]") set-tab tab)))
      tabs " ")
     "\n")))

(cl-defmethod brb-event-plan--tab-plan ((x ep))
  "Render plan tab for X."
  (let* ((wines (ep-wines x))
         (host (ep-host x))
         (price (or (vulpea-note-meta-get (ep-event x) "price" 'number) 0))
         (statement (ep-statement x)))

    ;; GENERAL
    (cl-flet ((set-host (&rest _)
                (let* ((candidates (or (ep-participants x)
                                       (vulpea-db-query-by-tags-some '("people"))))
                       (host (vulpea-select-from "Host" candidates :require-match t)))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set "host" host 'append)
                    (unless (-contains-p (-map #'vulpea-note-id (ep-participants x)) (vulpea-note-id host))
                      (vulpea-buffer-meta-set "participants" (cons host (ep-participants x)) 'append))
                    (save-buffer))
                  (ep-reload-event x))))
      (insert (propertize "Planning" 'face 'org-level-1) "\n\n")
      (insert
       (string-table
        :row-start "- "
        :pad-type '(right left left)
        :sep "  "
        :data
        `(("Host:"
           ,(buttonize (if (ep-host x) (vulpea-note-title (ep-host x)) "_____") #'set-host)
           "")
          ("Price:" ,(vulpea-meta-buttonize (ep-event x) "price" 'number (-partial #'ep-save-event x)
                      :default 0 :to-string #'brb-price-format)
           "")))
       "\n")
      (insert "\n"))

    (cl-flet ((set-planned-participants (&rest _)
                (let ((num (read-number "Planned participants: ")))
                  (setf (alist-get 'planned-participants (ep-data x))
                        num)
                  (ep-save-data x (ep-data x)))))
      (insert (propertize "⇾ Forecast" 'face 'org-level-2) "\n\n")
      (let* ((ps (alist-get 'planned-participants (ep-data x)))
             (debit (* (or ps 0)
                       price))
             (gain-public (- debit (alist-get 'credit-public statement)))
             (gain-real (- debit (alist-get 'credit-real statement))))
        (insert
         (string-table
          :row-start "- "
          :pad-type '(right left left)
          :sep "  "
          :data
          `(("Participants:"
             ,(buttonize (if ps (format "%2d" ps) "__") #'set-planned-participants)
             "")
            ("Debit:" ,(brb-price-format debit) "")
            ("Spending (total):"
             ,(brb-price-format (alist-get 'credit-public statement))
             ,(brb-price-format (alist-get 'credit-real statement)))
            ("Gain:"
             ,(propertize (brb-price-format gain-public)
               'face (if (>= gain-public 0) 'success 'error))
             ,(propertize (brb-price-format gain-real)
               'face (if (>= gain-real 0) 'success 'error)))))
         "\n"))
      (insert "\n"))

    (insert
     (propertize "⇾ Finances" 'face 'org-level-2)
     "\n\n"
     (string-table
      :row-start "- "
      :pad-type '(right left left)
      :sep "  "
      :data
      `(("Spending (shared):"
         ,(brb-price-format (alist-get 'spending-shared statement))
         "")
        ("Spending (wines):"
         ,(brb-price-format (alist-get 'spending-wines-public statement))
         ,(brb-price-format (alist-get 'spending-wines-real statement)))
        ("Spending (extra):"
         ,(brb-price-format (alist-get 'spending-extra-public statement))
         ,(brb-price-format (alist-get 'spending-extra-real statement)))
        ("Spending (total):"
         ,(brb-price-format (alist-get 'credit-public statement))
         ,(brb-price-format (alist-get 'credit-real statement)))
        ("Debit:"
         ,(brb-price-format (alist-get 'debit-base statement))
         "")
        ("Debit (extra):"
         ,(brb-price-format (alist-get 'debit-extra statement))
         "")
        ("Debit (total):"
         ,(brb-price-format (alist-get 'debit statement))
         "")
        ("Gain:"
         ,(propertize (brb-price-format (alist-get 'balance-public statement))
           'face (if (>= (alist-get 'balance-public statement) 0) 'success 'error))
         ,(propertize (brb-price-format (alist-get 'balance-real statement))
           'face (if (>= (alist-get 'balance-real statement) 0) 'success 'error)))))
     "\n"
     "\n")

    ;; WINES
    (cl-flet ((add-wine (&rest _)
                (let* ((wine (vulpea-select-from
                              "Wine"
                              (--remove
                               (-contains-p (-map #'vulpea-note-id wines) (vulpea-note-id it))
                               (vulpea-db-query-by-tags-every '("wine" "cellar")))
                              :require-match t))
                       (pricep (read-number "Price public: "
                                            (ignore-errors
                                              (->> (vulpea-note-meta-get-list wine "price")
                                                   (--filter (s-suffix-p brb-currency it))
                                                   (-map #'string-to-number)
                                                   (-max)))))
                       (pricer (read-number "Price real: "
                                            (ignore-errors
                                              (->> (or (vulpea-note-meta-get-list wine "price private")
                                                     (vulpea-note-meta-get-list wine "price"))
                                                 (--filter (s-suffix-p brb-currency it))
                                                 (-map #'string-to-number)
                                                 (-min)))))
                       (data (ep-data x)))
                  (setf (alist-get 'wines data)
                        (-snoc (alist-get 'wines data) `((id . ,(vulpea-note-id wine))
                                                         (price-public . ,pricep)
                                                         (price-real . ,pricer)
                                                         (type . "normal"))))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set "wines" (-concat wines (list wine)) 'append)
                    (save-buffer))
                  (ep-save-data x data)
                  (ep-reload-event x)))
              (remove-wine (id)
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set
                   "wines"
                   (--remove (string-equal (vulpea-note-id it) id) wines)
                   'append)
                  (save-buffer))
                (let ((data (ep-data x)))
                  (setf (alist-get 'wines data)
                        (--remove
                         (string-equal (alist-get 'id it) id)
                         (alist-get 'wines data)))
                  (brb-event-data-write (ep-event x) data))
                (ep-reload-event x))
              (reorder-wine (pos)
                (let ((ord (read-number "Order: ")))
                  (when (or (< ord 1) (> ord (length wines)))
                    (user-error "Order must be between %d and %d" 1 (length wines)))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set
                     "wines"
                     (->> wines
                          (-remove-at pos)
                          (-insert-at (- ord 1) (nth pos wines)))
                     'append)
                    (save-buffer))
                  (ep-reload-event x)))
              (edit-price-public (id)
                (let* ((price (read-number "Price public: "
                                           (ignore-errors
                                             (->> (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "price")
                                                  (--filter (s-suffix-p brb-currency it))
                                                  (-map #'string-to-number)
                                                  (-max)))))
                       (price (if (< price 0) nil price))
                       (data (ep-data x)))
                  (--update-first-by
                   (string-equal id (alist-get 'id it))
                   (progn
                     (setf (alist-get 'price-public it) price)
                     it)
                   nil
                   (alist-get 'wines data))
                  (ep-save-data x data)))
              (edit-price-real (id)
                (let* ((price (read-number "Price real: "
                                           (ignore-errors
                                             (->> (or (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "price private")
                                                      (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "price"))
                                                  (--filter (s-suffix-p brb-currency it))
                                                  (-map #'string-to-number)
                                                  (-min)))))
                       (price (if (< price 0) nil price))
                       (data (ep-data x)))
                  (--update-first-by
                   (string-equal id (alist-get 'id it))
                   (progn
                     (setf (alist-get 'price-real it) price)
                     it)
                   nil
                   (alist-get 'wines data))
                  (ep-save-data x data)))
              (edit-type (id)
                (let* ((type (completing-read "Type: "
                                              '("normal" "bonus" "extra")
                                              nil t))
                       (data (ep-data x))
                       (wine-data (--find (string-equal id (alist-get 'id it))
                                          (alist-get 'wines data))))
                  (if wine-data
                      (setf (alist-get 'type wine-data) type)
                    (setf (alist-get 'wines data)
                          (-snoc (alist-get 'wines data)
                                 `((id . ,id)
                                   (price-public . nil)
                                   (price-real . nil)
                                   (type . ,type)))))
                  (ep-save-data x data)))
              (edit-volume (id)
                (let* ((volume (read-number "Volume: "))
                       (data (ep-data x)))
                  (setf (alist-get 'wines data)
                        (--update-first-by
                         (string-equal id (alist-get 'id it))
                         (progn
                           (setf (alist-get 'volume it) volume)
                           it)
                         nil
                         (alist-get 'wines data)))
                  (ep-save-data x data))))
      (insert
       (propertize "⇾ Wines" 'face 'org-level-2) "\n"
       "\n"
       (string-table
        :header '("" "" "producer" "name" "year" "p public" "p real" "type" "volume")
        :pad-type '(right right right right left left left right left)
        :width '(nil nil 22 26 nil nil nil nil nil)
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
              (let* ((data (->> (ep-data x)
                                (assoc-default 'wines)
                                (-find (lambda (other)
                                         (string-equal (vulpea-note-id it)
                                                       (assoc-default 'id other)))))))
                (list
                 (buttonize "[x]" #'remove-wine (vulpea-note-id it))
                 (buttonize (format "[%d]" (+ 1 it-index)) #'reorder-wine it-index)
                 (vulpea-note-meta-get it "producer" 'note)
                 (vulpea-buttonize it (-rpartial #'vulpea-note-meta-get "name"))
                 (or (vulpea-note-meta-get it "vintage") "NV")
                 (buttonize (brb-price-format (or (assoc-default 'price-public data) 0))
                            #'edit-price-public (vulpea-note-id it))
                 (buttonize (brb-price-format (or (assoc-default 'price-real data) 0))
                            #'edit-price-real (vulpea-note-id it))
                 (buttonize (or (assoc-default 'type data) "[_]")
                            #'edit-type (vulpea-note-id it))
                 (buttonize (format "%d" (or (assoc-default 'volume data) 750))
                            #'edit-volume (vulpea-note-id it))))
              it)
             (-concat it
                      '(sep)
                      `((,(buttonize "[+]" #'add-wine)
                         ""
                         ""
                         ,(seq-length wines)
                         ""
                         ,(brb-price-format (alist-get 'spending-wines-public statement))
                         ,(brb-price-format (alist-get 'spending-wines-real statement))
                         ""
                         ""
                         "")))))
       "\n"
       "\n"))

    ;; SHARED SPENDINGS
    (cl-flet ((add-item (&rest _)
                (let ((item (read-string "Item: "))
                      (price (read-number "Price: "))
                      (amount (read-number "Amount: "))
                      (data (ep-data x)))
                  (setf (alist-get 'shared data)
                        (-snoc (alist-get 'shared data) `((item . ,item)
                                                          (amount . ,amount)
                                                          (price . ,price))))
                  (ep-save-data x data)))
              (remove-item (item)
                (let ((data (ep-data x)))
                  (setf (alist-get 'shared data)
                        (--remove (string-equal item (alist-get 'item it))
                                  (alist-get 'shared data)))
                  (ep-save-data x data)))
              (edit-price (item)
                (let* ((price (read-number "Price: "))
                       (data (ep-data x)))
                  (setf (alist-get 'price
                                   (--find (string-equal (alist-get 'item it) item)
                                           (alist-get 'shared data)))
                        price)
                  (ep-save-data x data)))
              (edit-amount (item)
                (let* ((amount (read-number "Amount: "))
                       (data (ep-data x)))
                  (setf (alist-get 'amount
                                   (--find (string-equal (alist-get 'item it) item)
                                           (alist-get 'shared data)))
                        amount)
                  (ep-save-data x data))))
      (insert
       (propertize "⇾ Shared Spendings" 'face 'org-level-2) "\n\n"
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
        (--> (alist-get 'shared (ep-data x))
             (--map
              (list
               (buttonize "[x]" #'remove-item (alist-get 'item it))
               (alist-get 'item it)
               (buttonize (brb-price-format (alist-get 'price it)) #'edit-price (alist-get 'item it))
               (buttonize (number-to-string (alist-get 'amount it)) #'edit-amount (alist-get 'item it))
               (brb-price-format (ceiling
                                  (* (alist-get 'amount it)
                                     (alist-get 'price it)))))
              it)
             (-concat
              it
              '(sep)
              `((,(buttonize "[+]" #'add-item)
                 "" "" "" ,(brb-price-format (alist-get 'spending-shared statement)))))))
       "\n\n"))

    ;; PARTICIPANTS
    (cl-flet ((add-participant (&rest _)
                (let ((p (vulpea-select-from
                          "Participant"
                          (->> (vulpea-db-query-by-tags-every '("people"))
                               (--remove
                                (-contains-p (-map #'vulpea-note-id (ep-participants x)) (vulpea-note-id it)))
                               (--remove
                                (-contains-p (-map #'vulpea-note-id (ep-waiting x)) (vulpea-note-id it))))
                          :require-match t)))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set "participants" (-snoc (ep-participants x) p) 'append)
                    (save-buffer))
                  (puthash (vulpea-note-id p)
                           (brb-ledger-balance-of (vulpea-note-id p)
                                                  (vulpea-utils-with-note (ep-event x)
                                                    (vulpea-buffer-prop-get "date")))
                           (ep-balances x))
                  (ep-reload-event x)))
              (remove-participant (id)
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set
                   "participants"
                   (--remove (string-equal (vulpea-note-id it) id) (ep-participants x))
                   'append)
                  (save-buffer))
                (ep-reload-event x)))
      (insert (propertize (format "⇾ Participants (%d)" (length (ep-participants x))) 'face 'org-level-2) "\n\n")
      (insert
       (string-table
        :header '("" "participant" "mode" "fee")
        :pad-type '(left right right left)
        :header-sep "-"
        :header-sep-start "|-"
        :header-sep-conj "-+-"
        :header-sep-end "-|"
        :row-start "| "
        :row-end " |"
        :sep " | "
        :data
        (cl-flet ((edit-fee (id)
                    (let* ((fee (read-number "Fee: "))
                           (fee (unless (= fee price) fee))
                           (data (ep-data x))
                           (pds (alist-get 'participants data)))
                      (unless pds
                        (setf (alist-get 'participants data)
                              `(((id . ,id)
                                 (fee . ,fee)))))
                      (if-let ((pd (--find (string-equal id (alist-get 'id it)) pds)))
                          (setf (alist-get 'fee pd) fee)
                        (setf (alist-get 'participants data)
                              (-snoc (alist-get 'participants data)
                                     `((id . ,id)
                                       (fee . ,fee)))))
                      (ep-save-data x data))))
          (-snoc
           (--map
            (let ((statement (ep-statement-for x it)))
              (list
               (buttonize "[x]" #'remove-participant (vulpea-note-id it))
               (vulpea-note-title it)
               (alist-get 'mode statement)
               (buttonize (brb-price-format (alist-get 'fee statement)) #'edit-fee (vulpea-note-id it))))
            (ep-participants x))
           'sep
           (list (buttonize "[+]" #'add-participant) "" "" (brb-price-format (alist-get 'debit-base statement))))))))
    (insert "\n\n")

    ;; WAITING LIST
    (cl-flet ((promote (pid)
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set
                   "participants" (-snoc (ep-participants x)
                                         (--find (string-equal pid (vulpea-note-id it))
                                                 (ep-waiting x))))
                  (vulpea-buffer-meta-set
                   "waiting" (--remove (string-equal pid (vulpea-note-id it)) (ep-waiting x)))
                  (save-buffer))
                (puthash pid
                         (brb-ledger-balance-of pid (vulpea-utils-with-note (ep-event x)
                                                      (vulpea-buffer-prop-get "date")))
                         (ep-balances x))
                (ep-reload-event x))
              (remove (pid)
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set
                   "waiting" (--remove (string-equal pid (vulpea-note-id it)) (ep-waiting x)))
                  (save-buffer))
                (ep-reload-event x))
              (add (&rest _)
                (let* ((known (->> (-concat (ep-participants x) (ep-waiting x))
                                   (-map #'vulpea-note-id)))
                       (cands (->> (vulpea-db-query-by-tags-every '("people"))
                                   (--remove (-contains-p known (vulpea-note-id it)))))
                       (p (vulpea-select-from "Person" cands :require-match t)))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set "waiting" (-snoc (ep-waiting x) p) 'append)
                    (save-buffer))
                  (ep-reload-event x))))
      (insert (propertize
             (format "⇾ Waiting list (%d)" (length (ep-waiting x)))
             'face 'org-level-2)
            "\n\n")
      (--each (ep-waiting x)
        (insert "- "
                (buttonize "[x]" #'remove (vulpea-note-id it))
                (buttonize "[↑]" #'promote (vulpea-note-id it))
                " "
                (vulpea-note-title it)
                "\n"))
      (insert "- "
              (buttonize "[+]" #'add)
              "\n"))))

(cl-defmethod brb-event-plan--tab-scores ((x ep))
  "Render scores tab for X."
  (let* ((participants (ep-participants x))
         (wines (ep-wines x))
         (summary (brb-event-summary (ep-event x))))
    (insert (propertize "Scores" 'face 'org-level-1) "\n\n")

    (insert (propertize "⇾ Summary" 'face 'org-level-2) "\n\n")
    (insert
     (string-table
      :row-start "- "
      :pad-type '(right left left)
      :sep "  "
      :data
      `(("Event RMS:" ,(if-let ((x (assoc-default 'rms summary)))
                           (format "%.4f" x)
                         "······"))
        ("Event WAVG:" ,(if-let ((x (assoc-default 'wavg summary)))
                            (format "%.4f" x)
                          "······"))
        ("Mean price (harmonic):" ,(brb-price-format (assoc-default 'wines-price-harmonic summary)))
        ("Mean price (median):" ,(brb-price-format (assoc-default 'wines-price-median summary)))
        ("Event QPR:" ,(if-let ((x (assoc-default 'qpr summary)))
                           (format "%.4f" x)
                         "······"))))
     "\n\n"
     (string-table
      :header '("" "producer" "wine" "year" "##" "wavg" "sdev" "qpr" "fav" "out")
      :pad-type '(left right right right right left left left right left)
      :width '(nil 20 36 nil nil nil nil 6 nil nil)
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "
      :data
      (--map-indexed
       (let* ((wid (vulpea-note-id (assoc-default 'wine it)))
              (reveal (not (->> (ep-data x)
                                (alist-get 'wines)
                                (--find (string-equal wid (assoc-default 'id it)))
                                (alist-get 'blind)))))
         (message "%s => %s" wid reveal)
         (list
          (+ 1 it-index)
          (if reveal
              (vulpea-note-title (vulpea-note-meta-get (assoc-default 'wine it) "producer" 'note))
            "********")
          (if reveal
              (vulpea-buttonize (assoc-default 'wine it) (lambda (x) (vulpea-note-meta-get x "name")))
            "********")
          (if reveal
              (or (vulpea-note-meta-get (assoc-default 'wine it) "vintage" 'number) "NV")
            "****")
          (->> (assoc-default 'wines summary)
               (--sort (>= (or (assoc-default 'wavg it) 0) (or (assoc-default 'wavg other) 0)))
               (-find-index (lambda (other)
                              (string-equal (vulpea-note-id (assoc-default 'wine it))
                                            (vulpea-note-id (assoc-default 'wine other)))))
               (+ 1))
          (if (assoc-default 'rms it)
              (format "%.4f" (assoc-default 'wavg it))
            "------")
          (if (assoc-default 'wavg it)
              (format "%.4f" (assoc-default 'sdev it))
            "------")
          (if (assoc-default 'qpr it)
              (format "%.4f" (assoc-default 'qpr it))
            "------")
          (if (assoc-default 'fav it)
              (format "%d" (assoc-default 'fav it))
            "-")
          (if (assoc-default 'out it)
              (format "%d" (assoc-default 'out it))
            "-")))
       (assoc-default 'wines summary)))
     "\n\n")

    (insert (propertize "⇾ Personal" 'face 'org-level-2) "\n\n")
    (insert
     (string-table
      :header (cons " " (-iota (length wines) 1))
      :pad-type (cons 'right (--map 'left (-iota (length wines) 1)))
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "
      ;; :header-sep "─"
      ;; :header-sep-start "├─"
      ;; :header-sep-conj "─┼─"
      ;; :header-sep-end "─┤"
      ;; :row-start "│ "
      ;; :row-end " │"
      ;; :sep " │ "
      :data
      (-concat
       (list
        (cons
         "include"
         (cl-flet ((toggle (id)
                     (let ((data (ep-data x)))
                       (setf (alist-get 'wines data)
                             (--update-first-by
                              (string-equal id (alist-get 'id it))
                              (progn
                                (setf (alist-get 'ignore-scores it) (not (alist-get 'ignore-scores it)))
                                it)
                              nil
                              (alist-get 'wines data)))
                       (ep-save-data x data))))
           (-map
            (lambda (wine)
              (buttonize
               (if-let* ((wds (alist-get 'wines (ep-data x)))
                         (wd (--find (string-equal (alist-get 'id it) (vulpea-note-id wine)) wds))
                         (ignore (alist-get 'ignore-scores wd)))
                   (if ignore "{-}" "{+}")
                 "{+}")
               #'toggle (vulpea-note-id wine)))
            wines)))
        (cons
         "reveal"
         (cl-flet ((toggle (id)
                     (let ((data (ep-data x)))
                       (setf (alist-get 'wines data)
                             (--update-first-by
                              (string-equal id (alist-get 'id it))
                              (progn
                                (setf (alist-get 'blind it) (not (alist-get 'blind it)))
                                it)
                              nil
                              (alist-get 'wines data)))
                       (ep-save-data x data))))
           (-map
            (lambda (wine)
              (buttonize
               (if-let* ((wds (alist-get 'wines (ep-data x)))
                         (wd (--find (string-equal (alist-get 'id it) (vulpea-note-id wine)) wds))
                         (blind (alist-get 'blind wd)))
                   (if blind "{-}" "{+}")
                 "{+}")
               #'toggle (vulpea-note-id wine)))
            wines))))
       '(sep)
       (cl-flet ((set-score (d)
                   (let ((score (read-number "Score: ")))
                     (ep-set-score x (alist-get 'wid d) (alist-get 'pid d) (unless (= 0 score) score))))
                 (set-sentiment (d)
                   (let ((sentiment (completing-read "Sentiment: " '("favourite" "outcast" "none"))))
                     (ep-set-sentiment x (alist-get 'wid d) (alist-get 'pid d)
                                       (unless (string-equal "none" sentiment) sentiment)))))
         (-flatten-n
          1
          (-map
           (lambda (participant)
             (list
              (cons
               (vulpea-note-meta-get participant "public name")
               (-map
                (lambda (wine)
                  (buttonize
                   (if-let* ((wines-data (alist-get 'wines (ep-data x)))
                             (wine-data (--find (string-equal (alist-get 'id it) (vulpea-note-id wine))
                                                wines-data))
                             (score-data (--find (string-equal (alist-get 'participant it) (vulpea-note-id participant))
                                                 (alist-get 'scores wine-data)))
                             (score (alist-get 'score score-data)))
                       (format "%.2f" score)
                     "____")
                   #'set-score `((wid . ,(vulpea-note-id wine))
                                 (pid . ,(vulpea-note-id participant)))))
                wines))
              (cons
               ""
               (-map
                (lambda (wine)
                  (buttonize
                   (let* ((wines-data (alist-get 'wines (ep-data x)))
                          (wine-data (--find (string-equal (alist-get 'id it) (vulpea-note-id wine))
                                             wines-data))
                          (score-data (--find (string-equal (alist-get 'participant it) (vulpea-note-id participant))
                                              (alist-get 'scores wine-data)))
                          (sentiment (alist-get 'sentiment score-data)))
                     (cond
                      ((string-equal "favourite" sentiment)
                       "fav")
                      ((string-equal "outcast" sentiment)
                       "out")
                      (t "```")))
                   #'set-sentiment `((wid . ,(vulpea-note-id wine))
                                     (pid . ,(vulpea-note-id participant)))))
                wines))
              'sep))
           participants)))))
     "\n\n")))

(cl-defmethod brb-event-plan--tab-order ((x ep))
  "Render food tab for X."
  (cl-flet* ((add-item-for (pid)
               (let* ((personal (alist-get 'personal (ep-data x)))
                      (name (completing-read "Item: " (--map (alist-get 'item it) personal)))
                      (price (or (alist-get 'price (--find (string-equal name (alist-get 'item it)) personal))
                                 (read-number "Price: ")))
                      (amount (read-number "Amount: "))
                      (def-order `((participant . ,pid)
                                   (amount . ,amount)))
                      (def-item `((item . ,name) (price . ,price) (orders ,def-order))))
                 (setf personal
                       (--update-first-by
                        (string-equal name (alist-get 'item it))
                        (progn
                          (setf (alist-get 'orders it)
                                (-update-first-by
                                 (lambda (order) (string-equal pid (alist-get 'participant order)))
                                 (lambda (order)
                                   (setf (alist-get 'amount order) amount)
                                   order)
                                 def-order
                                 (alist-get 'orders it)))
                          it)
                        def-item
                        personal))
                 (setf (alist-get 'personal (ep-data x)) personal)
                 (ep-save-data x (ep-data x))))
             (add-item (&rest _)
               (let* ((participant (vulpea-select-from "Person" (ep-participants x) :require-match t))
                      (pid (vulpea-note-id participant)))
                 (add-item-for pid)))
             (remove-item (d)
               (let* ((pid (alist-get 'pid d))
                      (item (alist-get 'item d))
                      (personal (alist-get 'personal (ep-data x))))
                 (setf personal
                       (if pid
                           (->> personal
                                (--update-first-by
                                 (string-equal item (alist-get 'item it))
                                 (progn
                                   (setf (alist-get 'orders it)
                                         (-remove (lambda (order)
                                                    (string-equal pid (alist-get 'participant order)))
                                                  (alist-get 'orders it)))
                                   it)
                                 nil)
                                (--filter (alist-get 'orders it)))
                         (--remove (string-equal item (alist-get 'item it)) personal)))
                 (setf (alist-get 'personal (ep-data x)) personal)
                 (ep-save-data x (ep-data x)))))
    (insert (propertize "Order" 'face 'org-level-1) "\n\n")
    (insert
     (string-table
      :header '("" "item" "price" "q" "total" "participants")
      :pad-type '(left right right left left right)
      :width '(nil 40 nil nil nil 36)
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "
      :data
      (let ((delivery-amount (->> (ep-data x)
                                  (alist-get 'personal)
                                  (--map (alist-get 'orders it))
                                  (-flatten-n 1)
                                  (--map (alist-get 'amount it))
                                  (-sum)))
            (delivery-total (->> (ep-data x)
                                 (alist-get 'personal)
                                 (--map (ceiling (* (->> (alist-get 'orders it)
                                                         (--map (alist-get 'amount it))
                                                         (-sum))
                                                    (alist-get 'price it))))
                                 (-sum))))
        (-snoc
         (->> (ep-data x)
              (alist-get 'personal)
              (--map
               (let* ((amount (->> (alist-get 'orders it)
                                   (--map (alist-get 'amount it))
                                   (-sum)))
                      (total (ceiling (* amount (alist-get 'price it)))))
                 (list
                  (buttonize "[x]" #'remove-item `((item . ,(alist-get 'item it))))
                  (alist-get 'item it)
                  (brb-price-format (alist-get 'price it))
                  amount
                  (brb-price-format total)
                  (->> (alist-get 'orders it)
                       (--map (alist-get 'participant it))
                       (--map (-find
                               (lambda (p)
                                 (string-equal it (vulpea-note-id p)))
                               (ep-participants x)))
                       (-map #'vulpea-note-title)
                       (s-join ", "))))))
         'sep
         `(,(buttonize "[+]" #'add-item)
           ""
           ""
           ,delivery-amount
           ,delivery-total
           "")))))
    (insert "\n\n")
    (--each (ep-participants x)
      (let* ((pid (vulpea-note-id it))
             ;; this must be a separate function
             (order (->> (ep-data x)
                         (alist-get 'personal)
                         (--map
                          (let* ((od (->> (alist-get 'orders it)
                                          (--find (string-equal pid (alist-get 'participant it)))))
                                 (amount (or (when od (alist-get 'amount od))
                                             0)))
                            `((item . ,(alist-get 'item it))
                              (price . ,(alist-get 'price it))
                              (amount . ,amount)
                              (total . ,(* amount (alist-get 'price it))))))
                         (--filter (> (alist-get 'amount it) 0)))))
        (insert (propertize (concat "⇾ " (vulpea-note-title it)) 'face 'org-level-2) "\n\n")
        (insert
         (string-table
          :header '("" "item" "price" "q" "total")
          :pad-type '(left right left left left)
          :width '(nil 40 nil nil nil)
          :header-sep "-"
          :header-sep-start "|-"
          :header-sep-conj "-+-"
          :header-sep-end "-|"
          :row-start "| "
          :row-end " |"
          :sep " | "
          :data
          (-snoc
           (--map
            (list
             (buttonize "[x]" #'remove-item `((pid . ,pid) (item . ,(alist-get 'item it))))
             (alist-get 'item it)
             (brb-price-format (alist-get 'price it))
             (alist-get 'amount it)
             (brb-price-format (alist-get 'price it)))
            order)
           'sep
           (list
            (buttonize "[+]" #'add-item-for pid)
            ""
            ""
            (->> order (--map (alist-get 'amount it)) (-sum))
            (->> order (--map (alist-get 'total it)) (-sum)))))
         "\n\n")))))

(cl-defmethod brb-event-plan--tab-extra ((x ep))
  "Render extra wines tab in X."
  (insert (propertize "Extra wines" 'face 'org-level-1) "\n\n")
  (let* ((wds (->> (ep-data x)
                   (assoc-default 'wines)
                   (--filter (string-equal "extra" (assoc-default 'type it)))))
         (wines (brb-event-wines (ep-event x)))
         (participants (brb-event-participants (ep-event x)))
         (extra-wines (->> wines
                           (--map
                            (let ((wd (-find (lambda (wd) (string-equal (alist-get 'id wd)
                                                                        (vulpea-note-id it)))
                                             wds)))
                              `((wine . ,it)
                                (wd . ,wd))))
                           (--filter (alist-get 'wd it)))))
    (cl-flet* ((edit-asking-price (wid)
                 (let ((price (read-number "Price: "))
                       (data (ep-data x)))
                   (setf (alist-get 'wines data)
                         (--update-first-by
                          (string-equal wid (alist-get 'id it))
                          (progn
                            (setf (alist-get 'price-asking it) price)
                            it)
                          nil
                          (alist-get 'wines data)))
                   (ep-save-data x data)))
               (toggle-participant (d)
                 (let* ((pid (alist-get 'pid d))
                        (wid (alist-get 'wid d))
                        (data (ep-data x)))
                   (setf (alist-get 'wines data)
                         (--update-first-by
                          (string-equal wid (alist-get 'id it))
                          (progn
                            (setf (alist-get 'participants it)
                                  (if (-contains-p (alist-get 'participants it) pid)
                                      (-remove-item pid (alist-get 'participants it))
                                    (cons pid (alist-get 'participants it))))
                            it)
                          nil
                          (alist-get 'wines data)))
                   (ep-save-data x data))))
      (--each extra-wines
        (let* ((wine (alist-get 'wine it))
               (wd (alist-get 'wd it))
               (wid (vulpea-note-id wine))
               (ps (alist-get 'participants wd))
               (price-asking (or (alist-get 'price-asking wd)
                                 (alist-get 'price-public wd)))
               (glass-price (ep--glass-price wd)))
          (insert (propertize (concat "⇾ " (vulpea-note-title wine)) 'face 'org-level-2) "\n\n")
          (insert
           (string-table
            :row-start "- "
            :pad-type '(right left)
            :sep "  "
            :data
            (-concat
             (list
              (list "Price public:" (brb-price-format (alist-get 'price-public wd)))
              (list "Price real:" (brb-price-format (alist-get 'price-real wd)))
              (list "Asking price:"
                    (buttonize
                     (brb-price-format price-asking)
                     #'edit-asking-price wid))
              (list "Participants:" (length ps))
              (list "Glass price:" (brb-price-format glass-price)))
             (--map
              (list
               (concat (vulpea-note-title it) ":")
               (buttonize
                (if (-contains-p ps (vulpea-note-id it))
                    "[+]"
                  "[-]")
                #'toggle-participant `((pid . ,(vulpea-note-id it))
                                       (wid . ,wid))))
              participants)))
           "\n\n"))))))

;; * old misc

(cl-defmethod brb-event-plan--tab-invoices ((x ep))
  "Render checkout tab in X."
  (cl-flet ((refresh-balances (&rest _)
              (ep-refresh (ep-set-balances x)))
            (charge-all (&rest _)
              (--each (ep-participants x)
                (unless (string-equal brb-event-narrator-id (vulpea-note-id it))
                  (let ((st (ep-statement-for x it))
                      (event (ep-event x))
                      (date (vulpea-utils-with-note (ep-event x)
                              (vulpea-buffer-prop-get "date"))))
                  (brb-ledger-charge
                         :convive it
                         :code (concat (vulpea-note-id event) ":" (vulpea-note-id it))
                         :amount (alist-get 'total st)
                         :date (date-to-time date)))))
              (message "Done."))
            (record-spendings (&rest _)
              (let ((statement (ep-statement x))
                    (event (ep-event x))
                    (date (vulpea-utils-with-note (ep-event x)
                            (vulpea-buffer-prop-get "date"))))
                (brb-ledger-record-txn
                 :amount (alist-get 'spending-wines-real statement)
                 :date (date-to-time date)
                 :comment (format "%s: wines" (vulpea-note-title event))
                 :code (concat (vulpea-note-id event) ":wines")
                 :account-to "personal:account"
                 :account-from "balance:assets")
                (brb-ledger-record-txn
                 :amount (alist-get 'spending-extra-real statement)
                 :date (date-to-time date)
                 :comment (format "%s: wines extra" (vulpea-note-title event))
                 :code (concat (vulpea-note-id event) ":wines-extra")
                 :account-to "personal:account"
                 :account-from "balance:assets")
                (brb-ledger-spend
                 :amount (alist-get 'spending-shared statement)
                 :date (date-to-time date)
                 :comment (format "%s: shared" (vulpea-note-title event))
                 :code (concat (vulpea-note-id event) ":shared"))
                (brb-ledger-spend
                 :amount (alist-get 'spending-order statement)
                 :date (date-to-time date)
                 :comment (format "%s: delivery" (vulpea-note-title event))
                 :code (concat (vulpea-note-id event) ":delivery"))
                (message "Done.")))
            (toggle-use-balance (&rest _)
              (let ((use-balance (pcase (or (vulpea-note-meta-get (ep-event x) "use balance") "true")
                                    ("true" "false")
                                    (_ "true"))))
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set "use balance" use-balance 'append)
                  (save-buffer))
                (ep-reload-event x)))
            (set-use-balance ))
    (insert (propertize "Invoices" 'face 'org-level-1) "\n")
    (insert
     (buttonize "[Refresh Balances]" #'refresh-balances)
     " "
     (buttonize "[Charge All]" #'charge-all)
     " "
     (buttonize "[Record Spendings]" #'record-spendings)
     "\n\n")
    (insert
     (string-table
        :row-start "- "
        :pad-type '(right left)
        :sep "  "
        :data
        `(("Use balance:"
           ,(buttonize
             (format "[%s]" (pcase (or (vulpea-note-meta-get (ep-event x) "use balance") "true")
                             ("true" "X")
                             (_ " ")))
             #'toggle-use-balance))))
     "\n")
    (insert "\n")
    (--each (ep-participants x)
      (let* ((participant it)
             (statement (ep-statement-for x it))
             (balance (alist-get 'balance statement))
             (balance-final (alist-get 'balance-final statement))
             (price (alist-get 'fee statement))
             (order (alist-get 'order statement))
             (extra (alist-get 'extra statement))
             (total (alist-get 'total statement))
             (paysfor (->> (ep-data x)
                           (alist-get 'participants)
                           (--find (string-equal
                                    (vulpea-note-id participant)
                                    (alist-get 'id it)))
                           (alist-get 'pays-for)
                           (-map (lambda (pid)
                                   (--find
                                    (string-equal pid (vulpea-note-id it))
                                    (ep-participants x))))))
             (paysfor-str
              (string-table
               :row-start "- "
               :pad-str " "
               :pad-type '(right left)
               :sep "  "
               :data
               (cl-flet ((remove-pays-for (pid)
                           (ep-remove-pays-for
                            x
                            participant
                            (->> (ep-participants x)
                                 (--find (string-equal pid (vulpea-note-id it)))))))
                 (when paysfor
                   (-map
                    (lambda (p)
                      (list p (buttonize "[x]" #'remove-pays-for (vulpea-note-id p))))
                    paysfor)))))
             (listing (string-table
                       :row-start "- "
                       :pad-str " "
                       :pad-type '(right left)
                       :sep "  "
                       :data
                       (-concat
                        (when (> balance 0)
                          (list (list "Starting balance" (brb-price-format balance))))
                        (list
                         (list (format "Event (x%.2f)" (+ 1 (seq-length paysfor)))
                               (brb-price-format price)))
                        (--map
                         (list
                          (format "%s (x%.2f)" (alist-get 'item it) (alist-get 'amount it))
                          (brb-price-format (alist-get 'total it)))
                         order)
                        (--map
                         (list
                          (format "%s (x%.2f)" (vulpea-note-title (alist-get 'wine it)) (alist-get 'amount it))
                          (brb-price-format (alist-get 'total it)))
                         extra)
                        (list
                         (list "Total" (brb-price-format total)))
                        (when (> balance 0)
                          (list
                           (list "Final balance" (brb-price-format balance-final))
                           (list "Due" (brb-price-format (alist-get 'due statement)))))))))
        (cl-flet ((kill-statement (&rest _)
                    (let ((event (ep-event x))
                          (narrator (vulpea-db-get-by-id brb-event-narrator-id)))
                      (with-temp-buffer
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
                         listing
                         "\n\n"
                         (if (> total 0)
                             (concat
                              "mono: " (vulpea-note-meta-get narrator "cc mono") "\n"
                              "ukrsib: " (vulpea-note-meta-get narrator "cc ukrsib") "\n"
                              "web: " (vulpea-note-meta-get narrator "send mono") "\n"
                              "\n")
                           "")
                         "🥂 Cheers! See you next time!"
                         "\n\n"
                         "P.S. Don't forget you have your personal page at https://barberry.io/convives/"
                         (vulpea-note-id it))
                        (goto-char (point-min))
                        (replace-regexp "  +" ": ")
                        (kill-new (buffer-substring (point-min) (point-max))))))
                  (add-pays-for (&rest _)
                    (let* ((person (vulpea-select-from
                                    "Person"
                                    (->> (ep-participants x)
                                         (--remove (string-equal (vulpea-note-id it)
                                                                 (vulpea-note-id participant))))
                                    :require-match t))
                           (pid (vulpea-note-id participant)))
                      (ep-add-pays-for x participant person))))
          (insert
           (propertize
            (concat
             "⇾ "
             (vulpea-note-title it)
             " "
             (buttonize "🖂" #'kill-statement))
            'face 'org-level-2)
           "\n\n"
           (propertize
            (concat "Pays for "
                    (buttonize "[+]" #'add-pays-for))
            'face 'org-level-3)
           "\n"
           (if (string-empty-p paysfor-str)
               ""
             (concat paysfor-str "\n"))
           "\n"
           (propertize "Invoice" 'face 'org-level-3)
           "\n"
           listing
           "\n\n"))))))



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

;; * Utils

(defun brb-event-plan--mask-maybe (sensitive str)
  "Mask STR when SENSITIVE is non-nil."
  (if sensitive
      str
    (s-repeat (length str) "*")))



(provide 'lib-brb-event-plan)
;;; lib-brb-event-plan.el ends here
