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



(defconst brb-order--buffer-name "*Wine Order*")
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
                              "vasyl"))

(defun brb-order--source-discount (source order)
  "Calculate discount for ORDER from SOURCE."
  (let ((bottles (--reduce-from (+ acc (brb-order-item-amount it)) 0 order)))
    (pcase source
      (`"goodwine" (if (>= bottles 6) 0.14 0.05))
      (`"sabotage" (cond
                    ((>= bottles 24) 0.12)
                    ((>= bottles 6) 0.06)
                    (t 0)))
      (_ 0))))

(defun brb-order--discounts (order)
  "Calculate discount values for ORDER."
  (let ((by-source (-group-by #'brb-order-item-source brb-order--data))
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

(defun brb-order-data-update (id fn &optional no-update)
  "Update item with ID using FN.

When NO-UPDATE is non-nil, the data is not written and the buffer
is not updated."
  (setf brb-order--data (--map (if (string-equal (brb-order-item-id it) id)
                                   (funcall fn it)
                                 it)
                               brb-order--data))
  (unless no-update
    (brb-order-data-write brb-order--data)
    (brb-order--buffer-populate brb-order--data brb-order--view brb-order--buffer)))

(defun brb-order-data-put (item &optional no-update)
  "Put ITEM to order data.

When NO-UPDATE is non-nil, the data is not written and the buffer
is not updated."
  (brb-order-data-update (brb-order-item-id item) (lambda (_) item) no-update))

(defun brb-order-data-rem (id &optional no-update)
  "Remove item with ID from order data.

When NO-UPDATE is non-nil, the data is not written and the buffer
is not updated."
  (setf brb-order--data (--remove (string-equal (brb-order-item-id it) id) brb-order--data))
  (unless no-update
    (brb-order-data-write brb-order--data)
    (brb-order--buffer-populate brb-order--data brb-order--view brb-order--buffer)))

(defun brb-order-data-get (id)
  "Get order item with ID."
  (--find (string-equal (brb-order-item-id it) id) brb-order--data))



;;;###autoload
(defun brb-order ()
  "Plan wine order."
  (interactive)
  (let ((buffer (get-buffer-create brb-order--buffer-name))
        (data (brb-order-data-read))
        (view 'all))
    (setf brb-order--buffer buffer)
    (setf brb-order--data data)
    (setf brb-order--view view)
    (brb-order--buffer-populate data view buffer)
    (pop-to-buffer buffer)))

(defun brb-order--buffer-populate (data view buffer)
  "Populate order BUFFER with DATA using VIEW."
  (with-current-buffer buffer
    (read-only-mode +1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (propertize "Wine Orders" 'face 'org-level-1)
       "\n\n"
       "actions: " (buttonize "[add]" #'brb-order--add)
       "\n"
       "view:    "
       (buttonize "[all]" #'brb-order--view-all)
       " "
       (buttonize (format "[source:%s]"
                          (pcase view
                            (`(source ,x) x)
                            (_ "_")))
                  #'brb-order--view-source)
       " "
       (buttonize (format "[buyer:%s]"
                          (pcase view
                            (`(buyer ,x) (vulpea-note-title (vulpea-db-get-by-id x)))
                            (_ "_")))
                  #'brb-order--view-buyer)
       "\n\n")
      (insert
       (pcase brb-order--view
         (`all (brb-order--format-items
                :items data
                :title "Everything"
                :hide nil))
         (`(source ,x)
          (brb-order--format-items
           :items (--filter (string-equal x (brb-order-item-source it)) data)
           :title (format "From %s" x)
           :hide '("source")))
         (`(buyer ,x)
          (brb-order--format-items
           :items (--filter (string-equal x (brb-order-item-buyer it)) data)
           :title (format "For %s" (vulpea-note-title (vulpea-db-get-by-id x)))
           :hide '("buyer"))))))))

(cl-defun brb-order--format-items (&key items title hide)
  "Format order ITEMS.

TITLE is title. Meh.

HIDE is a list of columns to hide."
  (let* ((discounts (brb-order--discounts items))
         (header '("" "sku" "source" "item" "amount" "price" "total" "buyer"))
         (idxs (->> (seq-length header)
                    (-iota)
                    (--remove (-contains-p hide (nth it header))))))
    (concat
     (propertize title 'face 'org-level-2)
     "\n\n"
     "export: " (buttonize "[table]" #'brb-order--export-table items)
     "\n\n"
     (string-table
      :data
      (append
       (-select-columns
        idxs
        (--map
         (list
          (buttonize "[X]" #'brb-order--remove (brb-order-item-id it))
          (buttonize (concat "[" (or (brb-order-item-sku it) " ") "]")
                     #'brb-order--edit-sku (brb-order-item-id it))
          (brb-order-item-source it)
          (if (brb-order-item-wine it)
              (vulpea-buttonize (vulpea-db-get-by-id (brb-order-item-wine it))
                                (lambda (_) (brb-order-item-name it)))
            (brb-order-item-name it))
          (buttonize (concat "[" (string-from (brb-order-item-amount it)) "]")
                     #'brb-order--edit-amount (brb-order-item-id it))
          (buttonize (concat "[" (brb-price-format (brb-order-item-price it)) "]")
                     #'brb-order--edit-price (brb-order-item-id it))
          (brb-price-format (brb-order-item-total it discounts))
          (vulpea-buttonize (vulpea-db-get-by-id (brb-order-item-buyer it))))
         items))
       (list
        'sep
        (-select-by-indices
         idxs
         (list "" "" "" ""
               (--reduce-from (+ acc (brb-order-item-amount it)) 0 items)
               ""
               (brb-price-format
                (--reduce-from (+ acc (brb-order-item-total it discounts)) 0 items))
               ""))))
      :header (-select-by-indices idxs header)
      :width (-select-by-indices idxs '(full full full 40 full full full full))
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "))))

(cl-defun brb-order--format-items-by (items
                                      &key
                                      group-by
                                      to-header-fn
                                      hide)
  "Format ITEMS grouped by selector defined as GROUP-BY.

TO-HEADER-FN formats header.

HIDE is a list of columns to hide."
  (declare (indent 1))
  (let* ((by-x (-group-by group-by items))
         (discounts (brb-order--discounts items))
         (header '("" "sku" "source" "item" "amount" "price" "total" "buyer"))
         (idxs (->> (seq-length header)
                    (-iota)
                    (--remove (-contains-p hide (nth it header))))))
    (mapconcat
     (lambda (it)
       (concat
        (propertize (funcall to-header-fn (car it)) 'face 'org-level-2)
        "\n\n"
        "export: " (buttonize "[table]" #'brb-order--export-table (cdr it))
        "\n\n"
        (string-table
         :data
         (append
          (-select-columns
           idxs
           (--map
            (list
             (buttonize "[X]" #'brb-order--remove (brb-order-item-id it))
             (buttonize (concat "[" (or (brb-order-item-sku it) " ") "]")
                        #'brb-order--edit-sku (brb-order-item-id it))
             (brb-order-item-source it)
             (if (brb-order-item-wine it)
                 (vulpea-buttonize (vulpea-db-get-by-id (brb-order-item-wine it))
                                   (lambda (_) (brb-order-item-name it)))
               (brb-order-item-name it))
             (buttonize (concat "[" (string-from (brb-order-item-amount it)) "]")
                        #'brb-order--edit-amount (brb-order-item-id it))
             (buttonize (concat "[" (brb-price-format (brb-order-item-price it)) "]")
                        #'brb-order--edit-price (brb-order-item-id it))
             (brb-price-format (brb-order-item-total it discounts))
             (vulpea-buttonize (vulpea-db-get-by-id (brb-order-item-buyer it))))
            (cdr it)))
          (list
           'sep
           (-select-by-indices
            idxs
            (list "" "" "" ""
                  (--reduce-from (+ acc (brb-order-item-amount it)) 0 (cdr it))
                  ""
                  (brb-price-format
                   (--reduce-from (+ acc (brb-order-item-total it discounts)) 0 (cdr it)))
                  ""))))
         :header (-select-by-indices idxs header)
         :width (-select-by-indices idxs '(full full full 40 full full full full))
         :header-sep "-"
         :header-sep-start "|-"
         :header-sep-conj "-+-"
         :header-sep-end "-|"
         :row-start "| "
         :row-end " |"
         :sep " | ")))
     by-x
     "\n\n")))



(defun brb-order--add (&rest _)
  "Add a wine to order."
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
                :buyer (vulpea-note-id buyer))))
    (add-to-list 'brb-order--data item 'append)
    (brb-order-data-write brb-order--data)
    (brb-order--buffer-populate brb-order--data brb-order--view brb-order--buffer)))

(defun brb-order--remove (id)
  "Remove item with ID from order."
  (brb-order-data-rem id))

(defun brb-order--edit-sku (id)
  "Edit SKU of item with ID."
  (let ((sku (read-string "SKU: ")))
    (brb-order-data-update id (lambda (it)
                                (setf (brb-order-item-sku it) sku)
                                it))))

(defun brb-order--edit-amount (id)
  "Edit amount of item with ID."
  (let ((amount (read-number "Amount: ")))
    (brb-order-data-update id (lambda (it)
                                (setf (brb-order-item-amount it) amount)
                                it))))

(defun brb-order--edit-price (id)
  "Edit price of item with ID."
  (let* ((item (brb-order-data-get id))
         (wine-id (brb-order-item-wine item))
         (prices (when wine-id
                   (--> wine-id
                        (vulpea-db-get-by-id it)
                        (vulpea-note-meta-get-list it "price")
                        (--filter (s-suffix-p brb-currency it) it))))
         (price (string-to-number (completing-read "Price: " prices))))
    (brb-order-data-update id (lambda (it)
                                (setf (brb-order-item-price it) price)
                                it))))



(defun brb-order--view-all (&rest _)
  "Set view to all."
  (setf brb-order--view 'all)
  (brb-order--buffer-populate brb-order--data brb-order--view brb-order--buffer))

(defun brb-order--view-source (&rest _)
  "Set view to specific source."
  (let ((source (completing-read "Source: " brb-order-sources nil t)))
    (setf brb-order--view (list 'source source))
    (brb-order--buffer-populate brb-order--data brb-order--view brb-order--buffer)))

(defun brb-order--view-buyer (&rest _)
  "Set view to specific source."
  (let ((buyer (vulpea-select-from "Buyer" (vulpea-db-query-by-tags-every '("people"))
                                   :require-match t)))
    (setf brb-order--view (list 'buyer (vulpea-note-id buyer)))
    (brb-order--buffer-populate brb-order--data brb-order--view brb-order--buffer)))



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

(defun brb-order--export-table (items)
  "Export ITEMS as table."
  (let* ((buffer (get-buffer-create (format "*Wine Order Export*")))
         (items (brb-order--export-items-prepare items))
         (discounts (brb-order--discounts items)))
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



(provide 'lib-brb-order)
;;; lib-brb-order.el ends here
