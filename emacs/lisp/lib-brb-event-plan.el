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

;; * Entry point

;;;###autoload
(defun brb-event-plan (&optional event)
  "Start planning session for barberry garden EVENT."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (buffer (buffer-generate (format "*%s*" (vulpea-note-title event)) 'unique)))
    (brb-event-plan--propagate-new
     (ep-make buffer event 'plan))
    (pop-to-buffer buffer)))

;; * Event Plan

(cl-defstruct (ep (:constructor ep--create)
                  (:copier nil))
  buffer
  event
  data
  tab
  host
  participants
  wines)

(defun ep-make (buffer event tab)
  "Create EP from BUFFER, EVENT and TAB."
  (let ((host (vulpea-note-meta-get event "host" 'note))
        (participants (brb-event-participants event))
        (wines (brb-event-wines event)))
    (ep--create :buffer buffer
                :event  event
                :data (brb-event-data-read event)
                :tab tab
                :host host
                :participants participants
                :wines wines)))

;; ** Refresh

(cl-defmethod ep-refresh ((x ep))
  "Refresh X."
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-reload ((x ep))
  "Reload X."
  (let ((event (vulpea-db-get-by-id (vulpea-note-id (ep-event x)))))
    (brb-event-plan--propagate-new
     (ep-make (ep-buffer x) event (ep-tab x)))))

;; ** Saving

(cl-defmethod ep-save-data ((x ep) data)
  "Save DATA and reload X."
  (setf (ep-data x) data)
  (brb-event-data-write (ep-event x) (ep-data x))
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-save-event ((x ep) (event vulpea-note))
  "Save EVENT and reload X."
  (brb-event-plan--propagate-new
   (ep-make (ep-buffer x) event (ep-tab x))))

;; ** Tabs

(cl-defmethod ep-set-tab ((x ep) tab)
  "Set TAB and reload X."
  (setf (ep-tab x) tab)
  (brb-event-plan--propagate-new x))

;; ** Financial calculations

(cl-defmethod ep-statement-for ((x ep) pid)
  "Return financial statement for participant of X.

PID is id of participant."
  (let* ((host (ep-host x))
         (host-id (when host (vulpea-note-id host)))
         (price (vulpea-note-meta-get (ep-event x) "price" 'number))
         (host-p (string-equal pid host-id))
         (fee (if host-p
                  0
                (or (->> (ep-data x)
                         (alist-get 'participants)
                         (--find (string-equal pid (alist-get 'id it)))
                         (alist-get 'fee))
                    price)))
         (mode (cond
                (host-p "host")
                ((/= fee price) "custom")
                (t "normal"))))
    `((fee . ,fee)
      (mode . ,mode))))

(cl-defmethod ep-statement ((x ep))
  "Return financial statement for X."
  (let* ((price (vulpea-note-meta-get (ep-event x) "price" 'number))
         (wines-normal (->> (ep-data x)
                            (assoc-default 'wines)
                            (--filter (string-equal "normal" (assoc-default 'type it)))))
         (wines-extra (->> (ep-data x)
                           (assoc-default 'wines)
                           (--filter (string-equal "extra" (assoc-default 'type it)))))

         (spending-shared (->> (ep-data x)
                               (assoc-default 'shared)
                               (--map (ceiling
                                       (* (assoc-default 'amount it)
                                          (assoc-default 'price it))))
                               (-sum)))
         (spending-wines-public (->> wines-normal
                                     (--map (assoc-default 'price-public it))
                                     (--filter it)
                                     (-sum)))
         (spending-wines-real (->> wines-normal
                                   (--map (assoc-default 'price-real it))
                                   (--filter it)
                                   (-sum)))
         (spending-extra-public (->> wines-extra
                                     (--map (assoc-default 'price-public it))
                                     (--filter it)
                                     (-sum)))
         (spending-extra-real (->> wines-extra
                                   (--map (assoc-default 'price-real it))
                                   (--filter it)
                                   (-sum)))
         (credit-public (+ spending-wines-public spending-extra-public spending-shared))
         (credit-real (+ spending-wines-real spending-extra-real spending-shared))
         (debit-base (->> (ep-participants x)
                          (--map (alist-get 'fee (ep-statement-for x (vulpea-note-id it))))
                          (-sum)))
         ;; TODO: build based on smaller primitives
         ;; Glass price calculation should be a separate function
         (debit-extra (->> wines-extra
                           (--map
                            (let* ((price (or (assoc-default 'price-asking it)
                                              (assoc-default 'price-public it)))
                                   (ps (assoc-default 'participants it))
                                   (glass-price (if ps (ceiling (/ price (float (length ps)))) 0)))
                              (* glass-price (- (length ps) 1))))
                           (-sum)))
         (debit (+ debit-base debit-extra))
         (balance-public (- debit credit-public))
         (balance-real (- debit credit-real)))
    `((spending-shared . ,spending-shared)
      (spending-wines-public . ,spending-wines-public)
      (spending-wines-real . ,spending-wines-real)
      (spending-extra-public . ,spending-extra-public)
      (spending-extra-real . ,spending-extra-real)
      (credit-public . ,credit-public)
      (credit-real . ,credit-real)
      (debit-base . ,debit-base)
      (debit-extra . ,debit-extra)
      (debit . ,debit)
      (balance-public . ,balance-public)
      (balance-real . ,balance-real))))

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
         (price (vulpea-note-meta-get (ep-event x) "price" 'number))
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
                  (ep-reload x)))
              (set-planned-participants (&rest _)
                (let ((num (read-number "Planned participants: ")))
                  (setf (alist-get 'planned-participants (ep-data x))
                        num)
                  (ep-save-data x (ep-data x)))))
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
          ("Participants:"
           ,(buttonize
             (if-let ((v (alist-get 'planned-participants (ep-data x))))
                 (format "%2d" v)
               "__")
             #'set-planned-participants)
           ,(format "(%2d)" (length (ep-participants x))))
          ("Price:" ,(vulpea-meta-buttonize (ep-event x) "price" 'number (-partial #'ep-save-event x)
                      :default 0 :to-string #'brb-price-format)
           "")
          ("Spending (shared):"
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
       "\n")
      (insert "\n"))

    ;; WINES
    (cl-flet ((add-wine (&rest _)
                (let* ((wine (vulpea-select-from
                              "Wine"
                              (--remove
                               (-contains-p (-map #'vulpea-note-id wines) (vulpea-note-id it))
                               (vulpea-db-query-by-tags-every '("wine" "cellar")))
                              :require-match t))
                       (pricep (read-number "Price public: "
                                            (->> (vulpea-note-meta-get-list wine "price")
                                                 (--filter (s-suffix-p brb-currency it))
                                                 (-map #'string-to-number)
                                                 (-max))))
                       (pricer (read-number "Price real: "
                                            (->> (or (vulpea-note-meta-get-list wine "price private")
                                                     (vulpea-note-meta-get-list wine "price"))
                                                 (--filter (s-suffix-p brb-currency it))
                                                 (-map #'string-to-number)
                                                 (-min))))
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
                  (ep-reload x)))
              (remove-wine (id)
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set
                   "wines"
                   (--remove (string-equal (vulpea-note-id it) id) wines)
                   'append)
                  (save-buffer))
                (ep-reload x))
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
                  (ep-reload x)))
              (edit-price-public (id)
                (let* ((price (read-number "Price public: "
                                           (->> (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "price")
                                                (--filter (s-suffix-p brb-currency it))
                                                (-map #'string-to-number)
                                                (-max))))
                       (data (ep-data x))
                       (wds (alist-get 'wines data))
                       (wd (--find (string-equal id (alist-get 'id it)) wds)))
                  (when wd
                    (setf (alist-get 'price-public wd) price))
                  (setq wds (--> (--remove (string-equal id (alist-get 'id it)) wds)
                                 (-snoc it (or wd `((id . ,id)
                                                    (price-public . ,price)
                                                    (price-real . nil)
                                                    (type . nil))))))
                  (setf (alist-get 'wines data) wds)
                  (ep-save-data x data)))
              (edit-price-real (id)
                (let* ((price (read-number "Price real: "
                                           (->> (or (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "price private")
                                                    (vulpea-note-meta-get-list (vulpea-db-get-by-id id) "price"))
                                                (--filter (s-suffix-p brb-currency it))
                                                (-map #'string-to-number)
                                                (-min))))
                       (data (ep-data x))
                       (wine-data (--find (string-equal id (alist-get 'id it))
                                          (alist-get 'wines data))))
                  (if wine-data
                      (setf (alist-get 'price-real wine-data) price)
                    (setf (alist-get 'wines data)
                          (-snoc (alist-get 'wines data)
                                 `((id . ,id)
                                   (price-public . nil)
                                   (price-real . ,price)
                                   (type . nil)))))
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
                  (ep-save-data x data))))
      (insert
       (propertize "⇾ Wines" 'face 'org-level-2) "\n"
       "\n"
       (string-table
        :header '("" "" "producer" "name" "year" "p public" "p real" "type")
        :pad-type '(right right right right left left left right)
        :width '(nil nil nil 32 nil nil nil nil)
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
                            #'edit-type (vulpea-note-id it))))
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
                (let ((participant (vulpea-select-from
                                    "Participant"
                                    (--remove
                                     (-contains-p (-map #'vulpea-note-id (ep-participants x)) (vulpea-note-id it))
                                     (vulpea-db-query-by-tags-every '("people")))
                                    :require-match t)))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set "participants" (-snoc (ep-participants x) participant) 'append)
                    (save-buffer))
                  (ep-reload x)))
              (remove-participant (id)
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set
                   "participants"
                   (--remove (string-equal (vulpea-note-id it) id) (ep-participants x))
                   'append)
                  (save-buffer))
                (ep-reload x)))
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
            (let ((statement (ep-statement-for x (vulpea-note-id it))))
              (list
               (buttonize "[x]" #'remove-participant (vulpea-note-id it))
               (vulpea-note-title it)
               (alist-get 'mode statement)
               (buttonize (brb-price-format (alist-get 'fee statement)) #'edit-fee (vulpea-note-id it))))
            (ep-participants x))
           'sep
           (list (buttonize "[+]" #'add-participant) "" "" (brb-price-format (alist-get 'debit-base statement))))))))))

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
      :header '("" "producer" "wine" "year" "##" "rms" "wavg" "qpr" "fav" "out")
      :pad-type '(left right right right right left left left right left)
      :width '(nil 20 36 nil nil nil nil nil nil nil)
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "
      :data
      (--map-indexed
       (list
        (+ 1 it-index)
        (vulpea-note-title (vulpea-note-meta-get (assoc-default 'wine it) "producer" 'note))
        (vulpea-buttonize (assoc-default 'wine it) (lambda (x) (vulpea-note-meta-get x "name")))
        (or (vulpea-note-meta-get (assoc-default 'wine it) "vintage" 'number) "NV")
        (->> (assoc-default 'wines summary)
             (--sort (>= (or (assoc-default 'wavg it) 0) (or (assoc-default 'wavg other) 0)))
             (-find-index (lambda (other)
                            (string-equal (vulpea-note-id (assoc-default 'wine it))
                                          (vulpea-note-id (assoc-default 'wine other)))))
             (+ 1))
        (if (assoc-default 'rms it)
            (format "%.4f" (assoc-default 'rms it))
          "------")
        (if (assoc-default 'wavg it)
            (format "%.4f" (assoc-default 'wavg it))
          "------")
        (if (assoc-default 'qpr it)
            (format "%.4f" (assoc-default 'qpr it))
          "------")
        (if (assoc-default 'fav it)
            (format "%d" (assoc-default 'fav it))
          "-")
        (if (assoc-default 'out it)
            (format "%d" (assoc-default 'out it))
          "-"))
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
                     (let* ((data (ep-data x))
                            (wd (--find (string-equal id (alist-get 'id it)) (alist-get 'wines data))))
                       (setf (alist-get 'ignore-scores wd) (not (alist-get 'ignore-scores wd)))
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
            wines))))
       '(sep)
       (cl-flet ((set-score (d)
                   (let* ((score (read-number "Score: "))
                          (score (unless (= 0 score) score))
                          (data (ep-data x))
                          (def-score `((participant . ,(assoc-default 'pid d))
                                       (score . ,score)
                                       (sentiment . nil))))
                     (setf (alist-get 'wines data)
                           (--update-first-by
                            (string-equal (assoc-default 'wid d) (alist-get 'id it))
                            (progn
                              (setf (alist-get 'scores it)
                                    (-update-first-by
                                     (lambda (sd) (string-equal (assoc-default 'pid d) (alist-get 'participant sd)))
                                     (lambda (sd)
                                       (setf (alist-get 'score sd) score)
                                       sd)
                                     def-score
                                     (alist-get 'scores it)))
                              it)
                            nil
                            (alist-get 'wines data)))
                     (ep-save-data x data)))
                 (set-sentiment (d)
                   (let* ((sentiment (completing-read "Sentiment: " '("favourite" "outcast" "none")))
                          (sentiment (unless (string-equal "none" sentiment) sentiment))
                          (data (ep-data x))
                          (def-score `((participant . ,(assoc-default 'pid d))
                                       (score . nil)
                                       (sentiment . ,sentiment))))
                     (setf (alist-get 'wines data)
                           (--update-first-by
                            (string-equal (assoc-default 'wid d) (alist-get 'id it))
                            (progn
                              (setf (alist-get 'scores it)
                                    (-update-first-by
                                     (lambda (sd) (string-equal (assoc-default 'pid d) (alist-get 'participant sd)))
                                     (lambda (sd)
                                       (setf (alist-get 'sentiment sd) sentiment)
                                       sd)
                                     def-score
                                     (alist-get 'scores it)))
                              it)
                            nil
                            (alist-get 'wines data)))
                     (ep-save-data x data))))
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
      :width '(nil 40 nil nil nil 42)
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
  (let* ((extra-ids (->> (ep-data x)
                         (assoc-default 'wines)
                         (--filter (string-equal "extra" (assoc-default 'type it)))
                         (--map (assoc-default 'id it))))
         (wines (brb-event-wines (ep-event x)))
         (participants (brb-event-participants (ep-event x)))
         (extra-wines (--filter (-contains-p extra-ids (vulpea-note-id it)) wines)))
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
        (let* ((wid (vulpea-note-id it))
               (wd (->> (ep-data x)
                        (alist-get 'wines)
                        (--filter (string-equal "extra" (assoc-default 'type it)))
                        (--find (string-equal wid (alist-get 'id it)))))
               (ps (alist-get 'participants wd))
               (price-asking (or (alist-get 'price-asking wd)
                                 (alist-get 'price-public wd)))
               (glass-price (if ps (ceiling (/ price-asking (float (length ps)))) 0)))
          (insert (propertize (concat "⇾ " (vulpea-note-title it)) 'face 'org-level-2) "\n\n")
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
  (let* ((participants (brb-event-participants (ep-event x)))
         (wines (brb-event-wines (ep-event x)))
         (price (vulpea-note-meta-get (ep-event x) "price" 'number)))
    (cl-flet ()
    (insert (propertize "Invoices" 'face 'org-level-1) "\n\n")
    (--each participants
      (let* ((pid (vulpea-note-id it))
             (pd (->> (ep-data x)
                      (alist-get 'participants)
                      (--find (string-equal pid (alist-get 'id it)))))
             ;; TODO this must take into account host price
             (price (or (alist-get 'fee pd)
                        price))
             ;; TODO this must be a separate function
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
                         (--filter (> (alist-get 'amount it) 0))))
             (extra (->> (ep-data x)
                         (alist-get 'wines)
                         (--filter (string-equal "extra" (assoc-default 'type it)))
                         (--filter (-contains-p (assoc-default 'participants it) pid))
                         (--map
                          (let* ((ps (alist-get 'participants it))
                                 (wid (alist-get 'id it))
                                 (price (or (alist-get 'price-asking it)
                                            (alist-get 'price-public it)))
                                 (glass-price (ceiling (/ price (float (length ps)))))
                                 (wine (--find (string-equal wid (vulpea-note-id it)) wines)))
                            `((glass-price . ,glass-price)
                              (wine . ,wine))))
                         (--filter (alist-get 'wine it))))
             (total (+ price
                       (-sum (--map (alist-get 'total it) order))
                       (-sum (--map (alist-get 'glass-price it) extra)))))
        (insert (propertize (concat "⇾ " (vulpea-note-title it)) 'face 'org-level-2) "\n\n")
        (insert
         (string-table
          :row-start "- "
          :pad-str " "
          :pad-type '(right left)
          :sep "  "
          :data
          (-concat
           (list
            (list "Balance" (brb-price-format 0))
            (list "Event (x1.00)" (brb-price-format price)))
           (--map
            (list
             (format "%s (x%.2f)" (alist-get 'item it) (alist-get 'amount it))
             (brb-price-format (alist-get 'total it)))
            order)
           (--map
            (list
             (format "%s (x1.00)" (vulpea-note-title (alist-get 'wine it)))
             (brb-price-format (alist-get 'glass-price it)))
            extra)
           (list
            (list "Total" (brb-price-format total)))))
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
