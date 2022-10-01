;;; lib-brb-charge.el --- Utilities to calculate price for event participation -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Sep 2022
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
;; Entry point is `brb-charge'.
;;
;;; Code:

(require 'lib-brb)



(defvar brb-charge--narrator-id "bc8aa837-3348-45e6-8468-85510966527a")
(defvar brb-charge--buffer-name "*Barberry Garden Charge*")
(defvar brb-charge--buffer nil)
(defvar brb-charge--event nil)
(defvar brb-charge--data nil)



(cl-defstruct brb-charge-data
  wines
  shared-items
  personal-items
  event-price)

(cl-defstruct brb-charge-item amount price)

(defun brb-charge-data--file (event)
  "Return path to data file of EVENT."
  (file-name-with-extension (vulpea-note-path event) "data.el"))

(defun brb-charge-data-read (event)
  "Read `brb-charge-data' for EVENT."
  (let ((file (brb-charge-data--file event)))
    (if (file-exists-p file)
        (with-temp-buffer
          (condition-case nil
	            (progn
	              (insert-file-contents file)
                (read (current-buffer)))
	          (error
	           (message "Could not read data from %s" file))))
      (make-brb-charge-data
       :wines (make-hash-table :test 'equal)
       :shared-items (make-hash-table :test 'equal)
       :personal-items (make-hash-table :test 'equal)))))

(defun brb-charge-data-write (event data)
  "Write DATA for EVENT."
  (let ((file (brb-charge-data--file event)))
    (with-temp-file file
      (let ((print-level nil)
	          (print-length nil))
	      (print data (current-buffer))))))

(defun brb-charge-data-deliveries (data)
  "Return deliveries from DATA.

Result is a hash table, where key is delivery item name and value
is a property list (:amount :participants :price)."
  (let ((deliveries (make-hash-table :test 'equal)))
    (-each (hash-table-keys (brb-charge-data-personal-items data))
      (lambda (participant-id)
        (let ((items (gethash participant-id (brb-charge-data-personal-items data))))
          (-each (hash-table-keys items)
            (lambda (item-name)
              (let ((item (gethash item-name items))
                    (plist (gethash item-name deliveries)))
                (puthash item-name
                         (list :amount (+ (brb-charge-item-amount item)
                                          (or (plist-get plist :amount) 0))
                               :participants (cons participant-id
                                                   (plist-get plist :participants))
                               :price (brb-charge-item-price item))
                         deliveries)))))))
    deliveries))



;;;###autoload
(defun brb-charge ()
  "Start a flow to charge people for event participation."
  (interactive)
  (let* ((buffer (get-buffer-create brb-charge--buffer-name))
         (event (brb-event-select))
         (data (brb-charge-data-read event)))
    (setf brb-charge--buffer buffer)
    (setf brb-charge--event event)
    (setf brb-charge--data data)
    (brb-charge--buffer-populate buffer event data)
    (pop-to-buffer buffer)))



(defun brb-charge--buffer-populate (buffer event data)
  "Populate BUFFER with EVENT DATA."
  (let* ((wines (brb-event-wines event))
         (date (vulpea-utils-with-note event
                 (vulpea-buffer-prop-get "date")))
         (date-next (time-add (date-to-time date)
                              (* 60 60 24)))
         (participants (brb-event-participants event))
         (participants-count (- (seq-length participants) 1))
         (wine-prices (--map
                       (gethash (vulpea-note-id it) (brb-charge-data-wines data))
                       wines))
         (wines-total (--reduce-from (+ acc (or it 0)) 0 wine-prices))
         (shared-total (--reduce-from
                        (+ acc (* (brb-charge-item-amount it) (brb-charge-item-price it)))
                        0
                        (hash-table-values (brb-charge-data-shared-items data))))
         (total (+ wines-total shared-total))
         (event-price-rec (ceiling (/ total (float participants-count))))
         (event-price (or
                       (brb-charge-data-event-price data)
                       event-price-rec)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert
       (propertize (vulpea-buttonize event) 'face 'org-level-1) "\n"
       "\n"
       (string-table
        :data `(("Participants" ,participants-count "(+ 1)")
                ("Wines" ,(brb-price-format wines-total) "")
                ("Shared" ,(brb-price-format shared-total) "")
                ("Total" ,(brb-price-format total) "")
                ("Event price (recommended)" ,(brb-price-format event-price-rec) "")
                ("Event price (actual)" ,(brb-price-format event-price)
                 ,(buttonize "[set]" #'brb-charge--set-event-price))
                ("Gain" ,(let ((v (- (* participants-count event-price) total)))
                          (propertize (brb-price-format v)
                           'face (if (>= v 0) 'success 'error)))
                 ""))
        :pad-type '(right left left)
        :pad-str (propertize "·" 'face 'barberry-theme-face-subtle)
        :sep (propertize "·" 'face 'barberry-theme-face-subtle)
        :row-start "- ")
       "\n"
       "\n"
       (propertize "Wines" 'face 'org-level-2) "\n"
       "\n"
       (string-table
        :data (append
               (--map-indexed
                (list
                 it
                 (if-let ((price (nth it-index wine-prices)))
                     (brb-price-format price)
                   "_______")
                 (buttonize "[set]" #'brb-charge--set-wine-price (vulpea-note-id it)))
                wines)
               `(("Total"
                  ,(propertize
                    (brb-price-format wines-total)
                    'face 'bold)
                  "")))
        :pad-type '(right left left)
        :pad-str (propertize "·" 'face 'barberry-theme-face-subtle)
        :sep (propertize "·" 'face 'barberry-theme-face-subtle)
        :row-start "- ")
       "\n"
       "\n"
       (propertize "Shared" 'face 'org-level-2) "\n"
       "\n"
       (buttonize "[add]" #'brb-charge--add-shared-item) "\n"
       "\n"
       (string-table
        :data (append
               (--map
                (let ((item (gethash it (brb-charge-data-shared-items data))))
                  (list
                   it
                   (brb-charge-item-price item)
                   "x"
                   (brb-charge-item-amount item)
                   "=>"
                   (brb-price-format
                    (* (brb-charge-item-amount item)
                       (brb-charge-item-price item)))
                   (buttonize "[set]" #'brb-charge--set-shared-item it)
                   (buttonize "[del]" #'brb-charge--delete-shared-item it)))
                (hash-table-keys (brb-charge-data-shared-items data)))
               `(("Total" "" "" "" ""
                  ,(propertize
                    (brb-price-format shared-total)
                    'face 'bold)
                  "" "")))
        :pad-type '(right left left left left left left left)
        :pad-str (propertize "·" 'face 'barberry-theme-face-subtle)
        :sep (propertize "·" 'face 'barberry-theme-face-subtle)
        :row-start "- ")
       "\n"
       "\n"
       (propertize "Delivery" 'face 'org-level-2)
       "\n\n"
       (let ((deliveries (brb-charge-data-deliveries data)))
         (string-table
          :data (append
                 (--map
                  (let ((plist (gethash it deliveries)))
                    (list
                     it
                     (plist-get plist :amount)
                     (plist-get plist :price)
                     (* (plist-get plist :amount)
                        (plist-get plist :price))
                     (string-join
                      (-map
                       #'vulpea-buttonize
                       (-remove
                        #'vulpea-note-primary-title
                        (vulpea-db-query-by-ids (plist-get plist :participants))))
                      ", ")))
                  (hash-table-keys deliveries))
                 (list
                  (list (buttonize "[add]" #'brb-charge--add-delivery-item) "" "" "" "")
                  (list "----" "" "" "" "")
                  (list (propertize "Total" 'face 'bold)
                        (--reduce-from
                         (+ acc (plist-get it :amount))
                         0
                         (hash-table-values deliveries))
                        ""
                        (--reduce-from
                         (+ acc (* (plist-get it :amount)
                                   (plist-get it :price)))
                         0
                         (hash-table-values deliveries))
                        "")))
          :header '("meal" "amount" "price" "total" "ordered by")
          :pad-type '(right left left left right)
          :header-sep "-"
          :header-sep-start "|-"
          :header-sep-conj "-+-"
          :header-sep-end "-|"
          :row-start "| "
          :row-end " |"
          :sep " | "))
       "\n\n"
       (mapconcat
        (lambda (participant)
          (let* ((personal (or (gethash (vulpea-note-id participant)
                                        (brb-charge-data-personal-items data))
                               (make-hash-table :test 'equal)))
                 (event-price (if (string-equal "bc8aa837-3348-45e6-8468-85510966527a"
                                                (vulpea-note-id participant))
                                  0
                                event-price))
                 (balance (brb-ledger-balance-of participant date-next))
                 (total (--reduce-from
                         (+ acc (* (brb-charge-item-amount it)
                                   (brb-charge-item-price it)))
                         (- event-price balance)
                         (hash-table-values personal))))
            (concat
             (propertize (vulpea-buttonize participant) 'face 'org-level-2)
             "\n\n"
             (buttonize "[add]" #'brb-charge--add-personal-item (vulpea-note-id participant))
             " "
             (buttonize "[del]" #'brb-charge--delete-personal-item (vulpea-note-id participant))
             "\n\n"
             "- Event: " (brb-price-format event-price) "\n"
             (if (> balance 0)
                 (concat
                  "- Prepaid: " (brb-price-format (- balance)) "\n")
               "")
             (mapconcat
              (lambda (it)
                (let ((item (gethash it personal)))
                  (format "- %s (x%.1f): %s\n"
                          it
                          (brb-charge-item-amount item)
                          (brb-price-format
                           (* (brb-charge-item-amount item)
                              (brb-charge-item-price item))))))
              (hash-table-keys personal))
             "- Total: " (brb-price-format total)
             "\n")))
        participants
        "\n")
       "\n"
       "\n")
      (goto-char (point-min))
      (read-only-mode +1))))

(defun brb-charge--commit ()
  "Commit data modifications and refresh buffer."
  (brb-charge-data-write brb-charge--event brb-charge--data)
  (let ((pos (point)))
    (brb-charge--buffer-populate
     brb-charge--buffer
     brb-charge--event
     brb-charge--data)
    (with-current-buffer brb-charge--buffer
      (goto-char pos))))



(defun brb-charge--set-wine-price (id)
  "Set price of wine with ID."
  (let* ((data brb-charge--data)
         (wine (vulpea-db-get-by-id id))
         (prices (vulpea-note-meta-get-list wine "price"))
         (prices (--filter (s-suffix-p brb-currency it) prices))
         (price (gethash id (brb-charge-data-wines data)))
         (price-new (completing-read "Price: "
                                     (-uniq
                                      (if price
                                          (cons (brb-price-format price) prices)
                                        prices)))))
    (puthash id (string-to-number price-new) (brb-charge-data-wines data))
    (brb-charge--commit)))



(defun brb-charge--add-shared-item (&rest _)
  "Add shared item."
  (let ((name (read-string "Name: "))
        (price (read-number "Price: "))
        (amount (read-number "Amount: ")))
    (puthash name (make-brb-charge-item
                   :amount amount
                   :price price)
             (brb-charge-data-shared-items brb-charge--data))
    (brb-charge--commit)))

(defun brb-charge--set-shared-item (name)
  "Update shared item with NAME."
  (let ((price (read-number "Price: "))
        (amount (read-number "Amount: ")))
    (puthash name (make-brb-charge-item
                   :amount amount
                   :price price)
             (brb-charge-data-shared-items brb-charge--data))
    (brb-charge--commit)))

(defun brb-charge--delete-shared-item (name)
  "Delete shared item with NAME."
  (remhash name (brb-charge-data-shared-items brb-charge--data))
  (brb-charge--commit))



(defun brb-charge--add-delivery-item (&rest _)
  "Add delivery item ."
  (let* ((participants (brb-event-participants brb-charge--event))
         (participant (vulpea-select-from "Participant" participants))
         (id (vulpea-note-id participant))
         (deliveries (brb-charge-data-deliveries brb-charge--data))
         (name (completing-read "Item: " (hash-table-keys deliveries)))
         (item (gethash name deliveries))
         (price (read-number "Price: " (when item (plist-get item :price))))
         (amount (read-number "Amount: "))
         (personal (or (gethash id (brb-charge-data-personal-items brb-charge--data))
                       (make-hash-table :test 'equal))))
    (puthash name (make-brb-charge-item
                   :amount amount
                   :price price)
             personal)
    (puthash id personal (brb-charge-data-personal-items brb-charge--data))
    (brb-charge--commit)))

(defun brb-charge--add-personal-item (id)
  "Add personal item to participant with ID."
  (let ((name (read-string "Name: "))
        (price (read-number "Price: "))
        (amount (read-number "Amount: "))
        (personal (or (gethash id (brb-charge-data-personal-items brb-charge--data))
                      (make-hash-table :test 'equal))))
    (puthash name (make-brb-charge-item
                   :amount amount
                   :price price)
             personal)
    (puthash id personal (brb-charge-data-personal-items brb-charge--data))
    (brb-charge--commit)))

(defun brb-charge--delete-personal-item (id)
  "Add personal item to participant with ID."
  (if-let ((items-tbl (brb-charge-data-personal-items brb-charge--data))
           (items (gethash id items-tbl)))
      (let ((item (completing-read "Item: " (hash-table-keys items) nil t))
            (personal (or (gethash id (brb-charge-data-personal-items brb-charge--data))
                          (make-hash-table :test 'equal))))
        (remhash item personal)
        (puthash id personal (brb-charge-data-personal-items brb-charge--data))
        (brb-charge--commit))
    (user-error "No personal items")))



(defun brb-charge--set-event-price (&rest _)
  "Set event price."
  (let ((price (read-number "Event price: ")))
    (setf (brb-charge-data-event-price brb-charge--data)
          price)
    (brb-charge--commit)))



(provide 'lib-brb-charge)
;;; lib-brb-charge.el ends here
