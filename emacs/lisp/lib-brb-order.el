;;; lib-brb-order.el --- Utilities to plan wine orders -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 10 Nov 2022
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

(require 'config-vulpea)
(require 'lib-brb)
(require 'tabulated-list)



(defvar brb-order-manager-buffer "*wine-order*")

(defvar-local brb-order-search-filter nil "Filter for order searches.")
(defvar-local brb-order-search--filter-source nil "Filter for order searches (source).")
(defvar-local brb-order-search--filter-buyer nil "Filter for order searches (buyer).")

(defvar-local brb-order--items nil "List of filtered items.")
(defvar-local brb-order--discounts nil "Hash map of discounts per source.")

(defvar brb-order--buffer)
(defvar brb-order--data)
(defvar brb-order--view 'all)

(cl-defstruct brb-order-item
  id
  sku
  source
  wine
  name
  amount
  price
  buyer)

(defun brb-order-item-total (item discounts)
  "Calculate total for ITEM based on DISCOUNTS."
  (let ((discount (if (and (string-equal "goodwine" (brb-order-item-source item))
                           (>= (brb-order-item-price item) 3300))
                      0.05
                    (gethash (brb-order-item-source item) discounts))))
    (ceiling
     (* (- 1.0 discount)
        (brb-order-item-amount item)
        (brb-order-item-price item)))))



(defconst brb-order-sources '("goodwine"
                              "sabotage"
                              "vasyl"
                              "maksym"
                              "roots"))

(defun brb-order--source-discount (source order)
  "Calculate discount for ORDER from SOURCE."
  (let ((bottles (--reduce-from (+ acc (brb-order-item-amount it)) 0 order)))
    (pcase source
      (`"goodwine" (if (>= bottles 6) 0.12 0.05))
      (`"roots" (if (>= bottles 6) 0.10 0.0))
      (`"sabotage" (cond
                    ((>= bottles 24) 0.12)
                    ((>= bottles 6) 0.06)
                    (t 0)))
      (_ 0))))

(defun brb-order--discounts (order)
  "Calculate discount values for ORDER."
  (let ((by-source (-group-by #'brb-order-item-source order))
        (tbl (make-hash-table :test 'equal)))
    (--each by-source
      (puthash (car it) (brb-order--source-discount (car it) (cdr it)) tbl))
    tbl))



(defconst brb-order-data-file
  (expand-file-name "brb-order.el"
                    vulpea-directory))

(defun brb-order-data-read ()
  "Read order data from `brb-order-data-file'."
  (let ((file brb-order-data-file))
    (when (file-exists-p file)
      (with-temp-buffer
        (condition-case nil
	          (progn
	            (insert-file-contents file)
              (read (current-buffer)))
	        (error
	         (message "Could not read data from %s" file)))))))

(defun brb-order-data-write (data)
  "Wite order DATA to `brb-order-data-file'."
  (let ((file brb-order-data-file))
    (with-temp-file file
      (let ((print-level nil)
	          (print-length nil))
	      (print data (current-buffer))))))



(defun brb-order--list-printer (id cols)
  "Propertize entries.

Consult with `tabulated-list-printer' for information about ID
and COLS."
  (setf (aref cols 0) (propertize (aref cols 0) 'face 'barberry-theme-face-faded))
  (setf (aref cols 1) (propertize (aref cols 1) 'face 'barberry-theme-face-faded))
  (setf (aref cols 2) (propertize (aref cols 2) 'face 'barberry-theme-face-salient))
  (setf (aref cols 6) (propertize (aref cols 6) 'face 'barberry-theme-face-faded))
  (tabulated-list-print-entry id cols))



;;;###autoload
(defun brb-order-manager ()
  "Display wine order UI."
  (interactive)
  (with-current-buffer (get-buffer-create brb-order-manager-buffer)
    (brb-order-mode)
    (let* ((order (brb-order-data-read))
           (discounts (brb-order--discounts order)))
      ;; cache order and discounts
      (setq brb-order--items order
            brb-order--discounts discounts)

      ;; configure columns
      (setq tabulated-list-format [("SKU" 5 t)
                                   ("Source" 8 t)
                                   ("Item" 40 t)
                                   ("Amount" 10 t . (:right-align t))
                                   ("Price" 10 t . (:right-align t))
                                   ("Total" 10 t . (:right-align t))
                                   ("Buyer" 20 t)])

      ;; setup custom header, but keep keybindings
      (setq header-line-format (brb-order--header-line)
            tabulated-list-use-header-line nil)
      (tabulated-list-init-header)

      ;; default sorting
      (setq tabulated-list-sort-key '("Item"))

      ;; setup entries
      (brb-order-search-reset))

    ;; switch to buffer
    (pop-to-buffer brb-order-manager-buffer
                   '((display-buffer-reuse-window display-buffer-same-window)))))



(defun brb-order--header-line ()
  "Set `header-line-format' to reflect query.

If PREFIX is non-nil it is displayed before the rest of the header-line."
  (let* ((order brb-order--items)
         (discounts brb-order--discounts))
    (list
     (concat
      "Wine Order"
      (propertize (format
                   " (%d matches, %d bottles, %s total, %s discount) "
                   (length tabulated-list-entries)
                   (--reduce-from (+ acc (brb-order-item-amount it)) 0 order)
                   (brb-price-format (--reduce-from (+ acc (brb-order-item-total it discounts)) 0 order))
                   (brb-price-format (-
                                      (--reduce-from (+ acc (* (brb-order-item-price it)
                                                               (brb-order-item-amount it)))
                                                     0 order)
                                      (--reduce-from (+ acc (brb-order-item-total it discounts)) 0 order))))
                  'face '(:weight bold))
      " "
      brb-order-search-filter))))



(defun brb-order-search-by-source (&optional source)
  "Search by SOURCE."
  (interactive)
  (let ((source (or source (completing-read "Source: " brb-order-sources
                                            nil 'require-match))))
    (setq brb-order-search--filter-source source)
    (brb-order-search--apply)))

(defun brb-order-search-by-buyer (&optional buyer)
  "Search by BUYER."
  (interactive)
  (let* ((buyer (if (stringp buyer) (vulpea-db-get-by-id buyer) buyer))
         (buyer (or buyer (vulpea-select-from "Buyer" (vulpea-db-query-by-tags-every '("people"))
                                              :require-match t))))
    (setq brb-order-search--filter-buyer buyer)
    (brb-order-search--apply)))

(defun brb-order-search-reset ()
  "Search reset search."
  (interactive)
  (setq brb-order-search--filter-source nil
        brb-order-search--filter-buyer nil)
  (brb-order-search--apply))

(defun brb-order-search--apply ()
  "Apply search."
  (let ((items (brb-order-data-read)))
    (setq brb-order-search-filter (string-join
                                   (list
                                    (when brb-order-search--filter-source
                                      (concat "#" brb-order-search--filter-source))
                                    (when brb-order-search--filter-buyer
                                      (concat "@" (vulpea-note-title brb-order-search--filter-buyer))))
                                   " ")
          brb-order--items (--filter
                            (and (or (not brb-order-search--filter-source)
                                     (string-equal brb-order-search--filter-source (brb-order-item-source it)))
                                 (or (not brb-order-search--filter-buyer)
                                     (string-equal (vulpea-note-id brb-order-search--filter-buyer)
                                                   (brb-order-item-buyer it))))
                            items)
          brb-order--discounts (brb-order--discounts items)
          tabulated-list-entries (--map
                                  (list
                                   (brb-order-item-id it)
                                   (vector
                                    (brb-order-item-sku it)
                                    (brb-order-item-source it)
                                    (brb-order-item-name it)
                                    (number-to-string (brb-order-item-amount it))
                                    (brb-price-format (brb-order-item-price it))
                                    (brb-price-format (brb-order-item-total it brb-order--discounts))
                                    (vulpea-note-title (vulpea-db-get-by-id (brb-order-item-buyer it)))))
                                  brb-order--items)
          header-line-format (brb-order--header-line)))
  (tabulated-list-print 'rembember-pos))



(defun brb-order-add ()
  "Add a wine to order."
  (interactive)
  (let* ((source (completing-read "Source: " brb-order-sources))
         (wine (vulpea-select-from "Wine" (vulpea-db-query-by-tags-every '("wine" "cellar"))))
         (id (vulpea-note-id wine))
         (name (vulpea-note-title wine))
         (sku (read-string "SKU: "))
         (prices (when id (->> (vulpea-note-meta-get-list wine "price")
                               (--filter (s-suffix-p brb-currency it)))))
         (price (string-to-number (completing-read "Price: " prices)))
         (amount (read-number "Amount: "))
         (buyer (vulpea-select-from "Buyer" (vulpea-db-query-by-tags-every '("people"))
                                    :require-match t))
         (item (make-brb-order-item
                :id (org-id-uuid)
                :sku sku
                :source source
                :wine id
                :name name
                :amount amount
                :price price
                :buyer (vulpea-note-id buyer)))
         (items (brb-order-data-read)))
    (push item items)
    (brb-order-data-write items)
    (brb-order-search--apply)))

(defun brb-order-copy ()
  "Copy item at point."
  (interactive)
  (let ((item (copy-brb-order-item (brb-order--item-at-point)))
        (amount (read-number "Amount: "))
        (buyer (vulpea-select-from "Buyer" (vulpea-db-query-by-tags-every '("people"))
                                   :require-match t))
        (items (brb-order-data-read)))
    (setf (brb-order-item-id item) (org-id-uuid))
    (setf (brb-order-item-amount item) amount)
    (setf (brb-order-item-buyer item) (vulpea-note-id buyer))
    (push item items)
    (brb-order-data-write items)
    (brb-order-search--apply)))

(defun brb-order-remove ()
  "Remove item at point."
  (interactive)
  (let ((item (brb-order--item-at-point))
        (items (brb-order-data-read)))
    (when (y-or-n-p "Delete item at point? ")
      (setf items (--remove (string-equal (brb-order-item-id it)
                                          (brb-order-item-id item))
                            items))
      (brb-order-data-write items)
      (brb-order-search--apply))))

(defmacro brb-order-defedit (name selector input-fn)
  "Define an edit command for column with NAME using SELECTOR.

New value is taken from calling INPUT-FN."
  (declare (debug t))
  `(defun ,(intern (format "brb-order-edit-%s" (s-downcase name))) ()
    ,(format "Edit %s value of item at point." name)
    (interactive)
    (let ((item (brb-order--item-at-point))
          (value (,(fun-unquote input-fn) ,(concat name ": ")))
          (items (brb-order-data-read)))
     (setf items (--map (if (string-equal
                             (brb-order-item-id it)
                             (brb-order-item-id item))
                            (progn
                              (setf (,(fun-unquote selector) it) value)
                              it)
                          it)
                  items))
     (brb-order-data-write items)
     (brb-order-search--apply))))



(defun brb-order--item-at-point ()
  "Return current item of UI line."
  (or (when-let ((id (get-text-property (point) 'tabulated-list-id)))
        (--find (string-equal (brb-order-item-id it) id) brb-order--items))
      (user-error "No package at point")))



(defun brb-order--export-items-prepare (items)
  "Prepare ITEMS for export."
  (let* ((items (--filter (> (brb-order-item-amount it) 0) items))
         (items (hash-table-values
                 (-reduce-from
                  (lambda (acc it)
                    (setq it (copy-brb-order-item it))
                    (when-let ((item (gethash (brb-order-item-name it) acc)))
                      (setf (brb-order-item-amount it)
                            (+ (brb-order-item-amount item)
                               (brb-order-item-amount it))))
                    (puthash (brb-order-item-name it) it acc)
                    acc)
                  (make-hash-table :test 'equal)
                  items)))
         (items (--sort (string> (brb-order-item-name it)
                                 (brb-order-item-name other))
                        items)))
    items))

(defun brb-order--export-table ()
  "Export current view as table."
  (interactive)
  (let* ((buffer (get-buffer-create (format "*Wine Order Export*")))
         (items (brb-order--export-items-prepare brb-order--items))
         (discounts brb-order--discounts))
    (with-current-buffer buffer
      (read-only-mode +1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (string-table
          :data
          (append
           (--map
            (list
             (brb-order-item-sku it)
             (brb-order-item-name it)
             (brb-order-item-amount it)
             (brb-price-format (brb-order-item-price it))
             (brb-price-format (brb-order-item-total it discounts)))
            items)
           (list
            'sep
            (list
             "" ""
             (--reduce-from (+ acc (brb-order-item-amount it)) 0 items)
             ""
             (brb-price-format
              (--reduce-from (+ acc (brb-order-item-total it discounts)) 0 items)))))
          :header '("sku" "item" "amount" "price" "total")
          :header-sep "-"
          :header-sep-start "|-"
          :header-sep-conj "-+-"
          :header-sep-end "-|"
          :row-start "| "
          :row-end " |"
          :sep " | "))))
    (pop-to-buffer buffer)))

(defun brb-order--export-csv ()
  "Export current view as CSV."
  (interactive)
  (let* ((buffer (get-buffer-create (format "*Wine Order Export*")))
         (items (brb-order--export-items-prepare brb-order--items))
         (discounts brb-order--discounts))
    (with-current-buffer buffer
      (read-only-mode +1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (string-table
          :data
          (append
           (--map
            (list
             (brb-order-item-sku it)
             (brb-order-item-name it)
             (brb-order-item-amount it)
             (brb-price-format (brb-order-item-price it))
             (brb-price-format (brb-order-item-total it discounts)))
            items)
           (list
            (list
             "" ""
             (--reduce-from (+ acc (brb-order-item-amount it)) 0 items)
             ""
             (brb-price-format
              (--reduce-from (+ acc (brb-order-item-total it discounts)) 0 items)))))
          :header '("sku" "item" "amount" "price" "total")
          :pad-type 'right
          :sep ","))))
    (pop-to-buffer buffer)))



(define-derived-mode brb-order-mode tabulated-list-mode "brb-order"
  "Major mode to manage wine orders."
  (setq tabulated-list-printer #'brb-order--list-printer))

(defvar brb-order-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "fs") #'brb-order-search-by-source)
    (define-key m (kbd "fb") #'brb-order-search-by-buyer)
    (define-key m (kbd "fr") #'brb-order-search-reset)
    (define-key m (kbd "ei") (brb-order-defedit "SKU" #'brb-order-item-sku #'read-string))
    (define-key m (kbd "es") (brb-order-defedit "Source" #'brb-order-item-source
                                                (lambda (x)
                                                  (completing-read
                                                   x brb-order-sources
                                                   nil 'require-match))))
    (define-key m (kbd "en") (brb-order-defedit "Name" #'brb-order-item-name #'read-string))
    (define-key m (kbd "ea") (brb-order-defedit "Amount" #'brb-order-item-amount #'read-number))
    (define-key m (kbd "ep") (brb-order-defedit "Price" #'brb-order-item-price #'read-number))
    (define-key m (kbd "eb") (brb-order-defedit "Buyer" #'brb-order-item-buyer
                                                (lambda (x)
                                                  (vulpea-note-id
                                                   (vulpea-select-from
                                                    x
                                                    (vulpea-db-query-by-tags-every '("people"))
                                                    :require-match t)))))
    (define-key m (kbd "Et") #'brb-order--export-table)
    (define-key m (kbd "Ec") #'brb-order--export-csv)
    (define-key m (kbd "a") #'brb-order-add)
    (define-key m (kbd "c") #'brb-order-copy)
    (define-key m (kbd "d") #'brb-order-remove)
    m)
  "Keymap for `brb-order-mode'.")



(provide 'lib-brb-order)
;;; lib-brb-order.el ends here
