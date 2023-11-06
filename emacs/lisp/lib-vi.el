;;; lib-vi.el --- Vino inventory, just what I need! -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 05 Nov 2023
;;
;; URL: https://github.com/d12frosted/
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
(require 'vulpea)
(require 'dash)

;; * Database

(defvar vi-db-file
  (expand-file-name "wine.db" vulpea-directory)
  "Location of inventory database file.

This file is source of truth, so better to keep it somewhere
safe.")

(defvar vi--db-connection nil)

(defun vi--db ()
  "Return connection to database.

Bootstrap DB if needed."
  (let* ((file vi-db-file)
         (exists (file-exists-p file))
         (db (or vi--db-connection (emacsql-sqlite file))))
    (setq vi--db-connection db)
    (unless exists
      (emacsql db [:create-table location ([(location-id integer :primary-key :autoincrement)
                                            (name :not-null :unique)])])
      (emacsql db [:create-table source ([(source-id integer :primary-key :autoincrement)
                                          (name :not-null :unique)])])
      (emacsql db [:create-table bottle ([(bottle-id integer :primary-key :autoincrement)
                                          (wine-id :not-null)
                                          (volume integer :not-null)
                                          (purchase-date :not-null)
                                          (price :not-null)
                                          (price-usd :not-null)
                                          (location-id :not-null)
                                          (source-id integer :not-null)
                                          (comment)]
                                         (:foreign-key [location-id] :references location [location-id])
                                         (:foreign-key [source-id] :references source [source-id]))])
      (emacsql db [:create-table transaction ([(transaction-id integer :primary-key :autoincrement)
                                               (bottle-id integer :not-null)
                                               ;; purchase, consume, move
                                               (transaction-type :not-null)
                                               (transaction-date :not-null)
                                               (destination-location-id integer)]
                                              (:foreign-key [bottle-id] :references bottle [bottle-id])
                                              (:foreign-key [destination-location-id] :references location [location-id]))]))
    db))

(cl-defun vi-bottle-purchase (&key wine-id
                                   volume
                                   date
                                   price
                                   price-usd
                                   location-id
                                   source-id
                                   comment)
  "Purchase a bottle.

- WINE-ID is a id of relevant `vulpea-note'.
- VOLUME is measured in milliliters (750 default).
- DATE is purchase date.
- PRICE is price of the purchase in any currency.
- PRICE-USD is price of the purchase in USD.
- LOCATION-ID is id of the initial location.
- SOURCE-ID is id of the source.
- COMMENT is optional."
  (let ((db (vi--db)))
    (emacsql-with-transaction db
      (emacsql db
               [:insert :into bottle [wine-id
                                      volume
                                      purchase-date
                                      price
                                      price-usd
                                      location-id
                                      source-id
                                      comment]
                        :values $v1]
               `([,wine-id
                  ,(or volume 750)
                  ,date
                  ,price
                  ,price-usd
                  ,location-id
                  ,source-id
                  ,comment]))
      (let ((bottle-id (caar (emacsql db [:select (funcall last_insert_rowid)]))))
        (emacsql db
                 [:insert :into transaction [bottle-id
                                             transaction-type
                                             transaction-date]
                          :values $v1]
                 `([,bottle-id purchase ,date]))))))

(cl-defun vi-bottle-consume (&key bottle-id date)
  "Consume BOTTLE-ID on a DATE."
  (emacsql (vi--db)
           [:insert :into transaction [bottle-id
                                       transaction-type
                                       transaction-date]
                    :values $v1]
           `([,bottle-id consume ,date])))

;; (defun vi-bottle-move ())

;; * Sources

(cl-defstruct vi-source
  id name)

(defun vi-sources-all ()
  "Return list of available locations."
  (->> (emacsql (vi--db) [:select [source-id name] :from source])
       (--map (make-vi-source
               :id (nth 0 it)
               :name (nth 1 it)))))

(defun vi-sources-add (name)
  "Create a new source with NAME."
  (let ((db (vi--db)))
    (emacsql db [:insert :into source [name] :values $v1]
             `([,name]))
    (make-vi-source
     :id (caar (emacsql db [:select (funcall last_insert_rowid)]))
     :name name)))

;; * Locations

(cl-defstruct vi-location
  id name)

(defun vi-locations-all ()
  "Return list of available locations."
  (->> (emacsql (vi--db) [:select [location-id name] :from location])
       (--map (make-vi-location
               :id (nth 0 it)
               :name (nth 1 it)))))

(defun vi-locations-add (name)
  "Create a new location with NAME."
  (let ((db (vi--db)))
    (emacsql db [:insert :into location [name] :values $v1]
             `([,name]))
    (make-vi-location
     :id (caar (emacsql db [:select (funcall last_insert_rowid)]))
     :name name)))

;; * Locations UI

(defvar vi-locations-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'vi-locations-ui-quit)
      (define-key map "g" #'vi-locations-ui-update)
      (define-key map "a" #'vi-locations-ui-add)))
  "Keymap for `vi-locations-ui-mode'.")

(define-derived-mode vi-locations-ui-mode tabulated-list-mode "vino-inventory-locations"
  "Major mode for listing location entries."
  (setq tabulated-list-printer #'vi-locations-ui--list-printer))

(defun vi-locations-ui--list-printer (id cols)
  "Propertize entries.

Consult with `tabulated-list-printer' for information about ID
and COLS."
  (setf (aref cols 0) (propertize (aref cols 0) 'face 'barberry-theme-face-strong))
  (tabulated-list-print-entry id cols))

(defun vi-locations-ui-quit ()
  "Quit from locations UI."
  (interactive)
  (quit-window))

(defun vi-locations-ui-update ()
  "Update locations entries."
  (interactive)
  (setq tabulated-list-entries (--map
                                (list
                                 (vi-location-id it)
                                 (vector
                                  (vi-location-name it)
                                  (number-to-string 0)
                                  "0 liters"
                                  "0 USD"))
                                (vi-locations-all)))
  (tabulated-list-print 'rembember-pos))

(defun vi-locations-ui-add ()
  "Add location."
  (interactive)
  (let ((name (read-string "Name: ")))
    (vi-locations-add name)
    (vi-locations-ui-update)))

;;;###autoload
(defun vi-locations-ui ()
  "Open UI for locations."
  (interactive)
  (let ((buffer (get-buffer-create "*vino-inventory-locations*")))
    (switch-to-buffer buffer)
    (unless (eq major-mode 'vi-locations-ui-mode)
      (vi-locations-ui-mode))
    (setq tabulated-list-format [("Name" 36 t)
                                 ("Bottles" 10 t . (:right-align t))
                                 ("Volume" 10 t . (:right-align t))
                                 ("Value" 10 t . (:right-align t))])
    (setq tabulated-list-sort-key '("Name"))
    (tabulated-list-init-header)
    (vi-locations-ui-update)))

;; * Transactions

(defun vi-total-in (wine-id)
  "Total income of WINE-ID."
  (caar
   (emacsql (vi--db)
            [:select (funcall count *)
                     :from [transaction]
                     :join bottle :on (= bottle:bottle-id transaction:bottle-id)
                     :where (= bottle:wine-id $s1)
                     :and (= transaction-type 'purchase)]
            wine-id)))

(defun vi-total-out (wine-id)
  "Total outcome of WINE-ID."
  (caar
   (emacsql (vi--db)
            [:select (funcall count *)
                     :from [transaction]
                     :join bottle :on (= bottle:bottle-id transaction:bottle-id)
                     :where (= bottle:wine-id $s1)
                     :and (= transaction-type 'consume)]
            wine-id)))

(defun vi-available-bottles-for (wine-id)
  "Return list of available bottles for WINE-ID."
  (->> (emacsql
        (vi--db)
        [:select
         [bottle:bottle-id
          bottle:wine-id
          bottle:volume
          bottle:purchase-date
          bottle:price
          bottle:price-usd
          bottle:comment
          location:name
          source:name]
         :from [bottle]
         :join location :on (= bottle:location-id location:location-id)
         :join source :on (= bottle:source-id source:source-id)
         :left-join (as [:select
                         [bottle-id
                          (as
                           (funcall sum
                                    [:case :when (= transaction-type 'purchase) :then 1
                                           :when (= transaction-type 'consume) :then -1
                                           :else 0
                                           :end])
                           total-amount)]
                         :from [transaction]
                         :group-by bottle-id]
                        t)
         :on (= bottle:bottle-id t:bottle-id)
         :where (and (> (funcall coalesce t:total-amount 0) 0)
                     (= bottle:wine-id $s1))]
        wine-id)
       (--map
        `((bottle-id . ,(nth 0 it))
          (wine-id . ,(nth 1 it))
          (volume . ,(nth 2 it))
          (purchase-date . ,(nth 3 it))
          (price . ,(nth 4 it))
          (price-usd . ,(nth 5 it))
          (comment . ,(nth 6 it))
          (location . ,(nth 7 it))
          (source . ,(nth 8 it))))))

(defun vi-available-wines ()
  "Return list of available wines."
  (->> (emacsql
        (vi--db)
        [:select
         [bottle:wine-id]
         :from [bottle]
         :left-join (as [:select
                         [bottle-id
                          (as
                           (funcall sum
                                    [:case :when (= transaction-type 'purchase) :then 1
                                           :when (= transaction-type 'consume) :then -1
                                           :else 0
                                           :end])
                           total-amount)]
                         :from [transaction]
                         :group-by bottle-id]
                        t)
         :on (= bottle:bottle-id t:bottle-id)
         :where (> (funcall coalesce t:total-amount 0) 0)])
       (-flatten-n 1)
       (-uniq)
       (vulpea-db-query-by-ids)))

;; * Inventory UI

(defvar vi-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'vi-ui-quit)
      (define-key map "g" #'vi-ui-update)))
  "Keymap for `vi-ui-mode'.")

(define-derived-mode vi-ui-mode tabulated-list-mode "vino-inventory"
  "Major mode for listing inventory entries."
  (setq tabulated-list-printer #'vi-ui--list-printer))

(defun vi-ui--list-printer (id cols)
  "Propertize entries.

Consult with `tabulated-list-printer' for information about ID
and COLS."
  (setf (aref cols 0) (propertize (aref cols 0) 'face 'barberry-theme-face-faded))
  (setf (aref cols 1) (propertize (aref cols 1) 'face 'barberry-theme-face-faded))
  (setf (aref cols 6) (propertize (aref cols 6) 'face 'barberry-theme-face-faded))
  (tabulated-list-print-entry id cols))

(defun vi-ui-quit ()
  "Quit from inventory UI."
  (interactive)
  (quit-window))

(defun vi-ui-update ()
  "Update inventory entries."
  (interactive)
  (let* ((db (vi--db))
         (bottles (emacsql
                   db
                   [:select
                    [bottle:bottle-id
                     bottle:wine-id
                     bottle:volume
                     bottle:purchase-date
                     bottle:price
                     (as location:name location-name)]
                    :from [bottle]
                    :join location :on (= bottle:location-id location:location-id)
                    :left-join (as [:select
                                    [bottle-id
                                     (as
                                      (funcall sum
                                               [:case :when (= transaction-type 'purchase) :then 1
                                                      :when (= transaction-type 'consume) :then -1
                                                      :else 0
                                                      :end])
                                      total-amount)]
                                    :from [transaction]
                                    :group-by bottle-id]
                                   t)
                    :on (= bottle:bottle-id t:bottle-id)
                    :where (> (funcall coalesce t:total-amount 0) 0)]))
         (wines-all (->> bottles
                         (--map (nth 1 it))
                         (-uniq)
                         (vulpea-db-query-by-ids)))
         (wines-tbl (let ((tbl (make-hash-table :test 'equal)))
                      (--each wines-all
                        (puthash (vulpea-note-id it) it tbl))
                      tbl)))
    (setq tabulated-list-entries
          (->> bottles
               (--sort (string< (vulpea-note-title (gethash (nth 1 it) wines-tbl))
                                (vulpea-note-title (gethash (nth 1 other) wines-tbl))))
               (--map
                (list
                 (number-to-string (nth 0 it))
                 (let ((wine (gethash (nth 1 it) wines-tbl)))
                   (vector
                    (number-to-string (nth 0 it))
                    (number-to-string (nth 2 it))
                    (string-match-n 2 org-link-bracket-re (vulpea-note-meta-get wine "producer"))
                    (vulpea-note-meta-get wine "name")
                    (or (vulpea-note-meta-get wine "vintage") "NV")
                    (nth 4 it)
                    (nth 3 it))))))))
  (tabulated-list-print 'rembember-pos))

(defun vi-ui-add ()
  "Add location."
  (interactive)
  (let ((name (read-string "Name: ")))
    (vi-locations-add name)
    (vi-ui-update)))

;;;###autoload
(defun vi-ui ()
  "Open inventory UI."
  (interactive)
  (let ((buffer (get-buffer-create "*vino-inventory*")))
    (switch-to-buffer buffer)
    (unless (eq major-mode 'vi-ui-mode)
      (vi-ui-mode))
    (setq tabulated-list-format [("ID" 5 t)
                                 ("Volume" 6 t . (:right-align t))
                                 ("Producer" 26 t . (:pad-right 2))
                                 ("Wine" 44 t . (:pad-right 2))
                                 ("Vintage" 8 t . (:right-align t))
                                 ("Price" 10 t . (:right-align t))
                                 ("Date" 10 t)])
    (setq tabulated-list-sort-key nil)
    (tabulated-list-init-header)
    (vi-ui-update)))

(provide 'lib-vi)
;;; lib-vi.el ends here
