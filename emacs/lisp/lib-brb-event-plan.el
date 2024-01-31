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

;; * New propagation flow

(cl-defstruct (ep (:constructor ep-create)
                  (:copier nil))
  buffer
  event
  data
  tab)

(defun brb-event-plan-new (&optional event)
  "Start planning session for barberry garden EVENT."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (buffer (buffer-generate (format "*%s*" (vulpea-note-title event)) 'unique)))
    (brb-event-plan--propagate-new
     (ep-create :buffer buffer
                :event  event
                :data (brb-event-data-read event)
                :tab 'plan))
    (pop-to-buffer buffer)))

(cl-defmethod ep-refresh ((x ep))
  "Refresh X."
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-reload ((x ep))
  "Reload X."
  (setf (ep-event x) (vulpea-db-get-by-id (vulpea-note-id (ep-event x))))
  (setf (ep-data x) (brb-event-data-read (ep-event x)))
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-save-data ((x ep) data)
  "Save DATA and reload X."
  (setf (ep-data x) data)
  (brb-event-data-write (ep-event x) (ep-data x))
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-save-event ((x ep) (event vulpea-note))
  "Save EVENT and reload X."
  (setf (ep-event x) event)
  (brb-event-plan--propagate-new x))

(cl-defmethod ep-set-tab ((x ep) tab)
  "Set TAB and reload X."
  (setf (ep-tab x) tab)
  (brb-event-plan--propagate-new x))

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
        (`invoices (brb-event-plan--tab-invoices x)))
      (insert "\n")

      (ws-butler-clean-region (point-min) (point-max))
      (goto-char point))))

(cl-defmethod brb-event-plan--header ((x ep))
  "Insert header for X."
  (let ((tabs '(plan scores order invoices))
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
  (let* ((participants (brb-event-participants (ep-event x)))
         (wines (brb-event-wines (ep-event x)))
         (host (vulpea-note-meta-get (ep-event x) "host" 'note))
         (shared-total (->> (ep-data x)
                            (assoc-default 'shared)
                            (--map (ceiling
                                    (* (assoc-default 'amount it)
                                       (assoc-default 'price it))))
                            (-sum)))
         (wines-total-public (->> (ep-data x)
                                  (assoc-default 'wines)
                                  (--filter (string-equal "normal" (assoc-default 'type it)))
                                  (--map (assoc-default 'price-public it))
                                  (--filter it)
                                  (-sum)))
         (wines-total-real (->> (ep-data x)
                                (assoc-default 'wines)
                                (--filter (string-equal "normal" (assoc-default 'type it)))
                                (--map (assoc-default 'price-real it))
                                (--filter it)
                                (-sum)))
         (credit-real (+ wines-total-real
                        shared-total))
         (credit-public (+ wines-total-public
                           shared-total))
         (debit (* (or (vulpea-note-meta-get (ep-event x) "price" 'number)
                       0)
                   (- (length participants) 1)))
         (price (vulpea-note-meta-get (ep-event x) "price" 'number))
         (debit (->> participants
                     (--map
                      (let ((pid (vulpea-note-id it)))
                        (if (and host (string-equal pid (vulpea-note-id host)))
                            0
                          (or (when-let ((pd (->> (ep-data x)
                                                  (alist-get 'participants)
                                                  (--find (string-equal pid (alist-get 'id it))))))
                                (alist-get 'fee pd))
                              price))))
                     (-sum)))
         (gain-real (- debit credit-real))
         (gain-public (- debit credit-public)))

    ;; GENERAL
    (cl-flet ((set-host (&rest _)
                (let* ((candidates (if (seq-empty-p participants)
                                       (vulpea-db-query-by-tags-some '("people"))
                                     participants))
                       (host (vulpea-select-from "Host" candidates :require-match t)))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set "host" host 'append)
                    (unless (-contains-p (-map #'vulpea-note-id participants) (vulpea-note-id host))
                      (vulpea-buffer-meta-set "participants" (cons host participants) 'append))
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
           ,(buttonize (if host (vulpea-note-title host) "_____") #'set-host)
           "")
          ("Participants:"
           ,(buttonize
             (if-let ((v (alist-get 'planned-participants (ep-data x))))
                 (format "%2d" v)
               "__")
             #'set-planned-participants)
           ,(format "(%2d)" (length participants)))
          ("Price:" ,(vulpea-meta-buttonize (ep-event x) "price" 'number (-partial #'ep-save-event x)
                      :default 0 :to-string #'brb-price-format)
           "")
          ("Spending (shared):"
           ,(brb-price-format shared-total)
           "")
          ("Spending (wines):"
           ,(brb-price-format wines-total-public)
           ,(brb-price-format wines-total-real))
          ("Spending (total):"
           ,(brb-price-format credit-public)
           ,(brb-price-format credit-real))
          ("Debit:"
           ,(brb-price-format debit)
           "")
          ("Gain:"
           ,(propertize (brb-price-format gain-public) 'face (if (>= gain-public 0) 'success 'error))
           ,(propertize (brb-price-format gain-real) 'face (if (>= gain-real 0) 'success 'error)))))
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
                         ,(brb-price-format wines-total-public)
                         ,(brb-price-format wines-total-real)
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
                 "" "" "" ,(brb-price-format shared-total))))))
       "\n\n"))

    ;; PARTICIPANTS
    (cl-flet ((add-participant (&rest _)
                (let ((participant (vulpea-select-from
                                    "Participant"
                                    (--remove
                                     (-contains-p (-map #'vulpea-note-id participants) (vulpea-note-id it))
                                     (vulpea-db-query-by-tags-every '("people")))
                                    :require-match t)))
                  (vulpea-utils-with-note (ep-event x)
                    (vulpea-buffer-meta-set "participants" (-concat participants (list participant)) 'append)
                    (save-buffer))
                  (ep-reload x)))
              (remove-participant (id)
                (vulpea-utils-with-note (ep-event x)
                  (vulpea-buffer-meta-set
                   "participants"
                   (--remove (string-equal (vulpea-note-id it) id) participants)
                   'append)
                  (save-buffer))
                (ep-reload x)))
      (insert (propertize (format "⇾ Participants (%d)" (length participants)) 'face 'org-level-2) "\n\n")
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
            (let* ((pid (vulpea-note-id it))
                   (pd (->> (ep-data x)
                            (alist-get 'participants)
                            (--find (string-equal pid (alist-get 'id it)))))
                   (mode (cond
                          ((and host (string-equal pid (vulpea-note-id host))) "host")
                          ((and pd (/= (or (alist-get 'fee pd) price) price)) "custom")
                          (t "normal")))
                   (fee (pcase mode
                          (`"host" 0)
                          (_ (or (when pd (alist-get 'fee pd))
                                 price)))))
              (list
               (buttonize "[x]" #'remove-participant (vulpea-note-id it))
               (vulpea-note-title it)
               mode
               (buttonize (brb-price-format fee) #'edit-fee pid)))
            participants)
           'sep
           (list (buttonize "[+]" #'add-participant) "" "" (brb-price-format debit)))))))))

(cl-defmethod brb-event-plan--tab-scores ((x ep))
  "Render scores tab for X."
  (let* ((participants (brb-event-participants (ep-event x)))
         (wines (brb-event-wines (ep-event x)))
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
          "-")
        (if (assoc-default 'wavg it)
            (format "%.4f" (assoc-default 'wavg it))
          "-")
        (if (assoc-default 'qpr it)
            (format "%.4f" (assoc-default 'qpr it))
          "-")
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
                          (wid (assoc-default 'wid d))
                          (pid (assoc-default 'pid d))
                          (data (ep-data x))
                          (wds (alist-get 'wines data))
                          (wd (or (--find (string-equal wid (alist-get 'id it)) wds)
                                  (error "No wine data for %s" wid)))
                          (sds (assoc 'scores wd))
                          (sd (--find (string-equal pid (alist-get 'participant it)) (cdr sds))))
                     (if sd
                         (setf (alist-get 'score sd) score)
                       (setcdr sds (append (cdr sds) (list `((participant . ,pid)
                                                             (score . ,score)
                                                             (sentiment . nil))))))
                     (ep-save-data x data)))
                 (set-sentiment (d)
                   (let* ((sentiment (completing-read "Sentiment: " '("favourite" "outcast" "none")))
                          (sentiment (unless (string-equal "none" sentiment) sentiment))
                          (wid (assoc-default 'wid d))
                          (pid (assoc-default 'pid d))
                          (data (ep-data x))
                          (wds (alist-get 'wines data))
                          (wd (or (--find (string-equal wid (alist-get 'id it)) wds)
                                  (error "No wine data for %s" wid)))
                          (sds (assoc 'scores wd))
                          (sd (--find (string-equal pid (alist-get 'participant it)) (cdr sds))))
                     (if sd
                         (setf (alist-get 'sentiment sd) sentiment)
                       (setcdr sds (append (cdr sds) (list `((participant . ,pid)
                                                             (score . nil)
                                                             (sentiment . ,sentiment))))))
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
                     "___")
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
  (message "%S" x)
  (insert "this is food tab"))

(cl-defmethod brb-event-plan--tab-invoices ((x ep))
  "Render checkout tab in X."
  (message "%S" x)
  (insert "this is checkout tab"))

;; * Old flow

;;;###autoload
(defun brb-event-plan (&optional event)
  "Start planning session for barberry garden EVENT."
  (interactive)
  (let* ((event (or event (brb-event-select)))
         (data (brb-event-plan--data-read event))
         (buffer (buffer-generate (format "*%s*" (vulpea-note-title event)) 'unique)))
    (brb-event-plan--propagate buffer event data (make-hash-table :test 'equal) nil)
    (pop-to-buffer buffer)))



(defun brb-event-plan--propagate (buffer event data balances sensitive)
  "Propagate planning BUFFER for EVENT.

DATA is a properly list containing information about shared and
personal spendings.

BALANCES is a hash-table, where key is participant id and value
is balance.

When SENSITIVE is non-nil, show sensitive content."
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
                     buffer (vulpea-db-get-by-id (vulpea-note-id event)) (brb-event-plan--data-read event) balances sensitive)))
       " "
       (buttonize (concat "[Sensitive: " (if sensitive "on" "off") "]")
                  (lambda (&rest _)
                    (brb-event-plan--propagate
                     buffer (vulpea-db-get-by-id (vulpea-note-id event)) (brb-event-plan--data-read event) balances (not sensitive))))
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
                 (brb-event-plan--propagate buffer event data balances sensitive)))
             "")
            ("Price:" ,(vulpea-meta-buttonize event "price" 'number
                        (lambda (event) (brb-event-plan--propagate buffer event data balances sensitive))
                        :default 0
                        :to-string #'brb-price-format)
             "")
            ("Spending (shared):"
             ,(plist-buttonize-prop data :planned-shared-spending 0
               (lambda (data)
                 (brb-event-plan--data-write event data)
                 (brb-event-plan--propagate buffer event data balances))
               (-compose (-partial #'brb-event-plan--mask-maybe sensitive) #'brb-price-format))
             "")
            ("Spending (wines):"
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format wines-total-public))
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format (plist-get invoice :wines-total))))
            ("Spending (total):"
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format total-public))
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format total-real)))
            ("Price recommended (0%):"
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format price-public))
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format price-real)))
            ("Price recommended (10%):"
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format (ceiling (* price-public 1.1))))
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format (ceiling (* price-real 1.1)))))
            ("Price recommended (25%):"
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format (ceiling (* price-public 1.25))))
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format (ceiling (* price-real 1.25)))))
            ("Price recommended (33%):"
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format (ceiling (* price-public 1.33))))
             ,(brb-event-plan--mask-maybe sensitive (brb-price-format (ceiling (* price-real 1.33)))))
            ("Planned debit:" ,(brb-event-plan--mask-maybe sensitive (brb-price-format debit)) "")
            ("Planned gain:"
             ,(brb-event-plan--mask-maybe sensitive
               (propertize (brb-price-format gain-public) 'face (if (>= gain-public 0) 'success 'error)))
             ,(brb-event-plan--mask-maybe sensitive
               (propertize (brb-price-format gain-real) 'face (if (>= gain-real 0) 'success 'error)))))))
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
               (brb-event-plan--propagate buffer event data balances sensitive))
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
                      (lambda (event) (brb-event-plan--propagate buffer event data balances seq-length))
                      :default 0
                      :to-string #'brb-price-format))
          ("Spending (shared):" ,(brb-event-plan--mask-maybe sensitive (brb-price-format (plist-get invoice :shared-total))))
          ("Spending (wines):" ,(brb-event-plan--mask-maybe sensitive (brb-price-format (plist-get invoice :wines-total))))
          ("Spending (total):" ,(brb-event-plan--mask-maybe sensitive (brb-price-format (plist-get invoice :credit))))
          ("Gain:" ,(let ((gain (plist-get invoice :balance)))
                     (brb-event-plan--mask-maybe sensitive (propertize (brb-price-format gain) 'face (if (>= gain 0) 'success 'error)))))))
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
                               buffer (vulpea-db-get-by-id (vulpea-note-id event)) (brb-event-plan--data-read event) balances sensitive)))))
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
                               buffer (vulpea-db-get-by-id (vulpea-note-id event)) (brb-event-plan--data-read event) balances sensitive)))
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
                      (brb-event-plan--mask-maybe sensitive (brb-price-format (or (nth it-index (plist-get wine-prices :real)) 0)))))
              it)
             (-concat it
                      '(sep)
                      `((""
                         ""
                         ,(seq-length wines)
                         ""
                         ,(brb-price-format wines-total-public)
                         ,(brb-event-plan--mask-maybe sensitive (brb-price-format (plist-get invoice :wines-total))))))))
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
                               (brb-event-plan--propagate buffer event data balances sensitive)))))
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
                            (brb-event-plan--propagate buffer event data balances sensitive)))
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
                               (brb-event-plan--propagate buffer event data balances sensitive)))))
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

;; * Utils

(defun brb-event-plan--mask-maybe (sensitive str)
  "Mask STR when SENSITIVE is non-nil."
  (if sensitive
      str
    (s-repeat (length str) "*")))



(provide 'lib-brb-event-plan)
;;; lib-brb-event-plan.el ends here
