;;; lib-vino-stats.el --- Vino statistics -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 18 Apr 2022
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
;; This module contains multiple utilities for `vino' statistics
;; calculations.
;;
;; See `vino-stats' function as an entry point.
;;
;; TODO:
;;
;; - find wines with the biggest ratings count
;;
;; - calculate tendency (for example, based on same "time-frame - 1")
;;
;; - avoid unnecessary conversions between calc and regular number
;;   format
;;
;; - calculate consumed and rated count in stats per country/vintage/colour/...
;;
;; - calculate stats per region
;;
;;; Code:

(require 'lib-buffer)
(require 'lib-string)
(require 'lib-vulpea)
(require 'lib-table)
(require 'lib-brb)
(require 'lib-calc)

(require 'vino)


;; configurations

(defvar vino-stats-price-currency "UAH"
  "Currency to use for price calculations.

All other currencies are ignored.")


;; time frames

(defconst vino-stats-time-frames '(this-year
                                   this-year-full
                                   this-month
                                   this-week
                                   today
                                   365-days
                                   30-days
                                   7-days
                                   eternity)
  "List of supported time frames.")

(defun vino-stats--time-frame-range (frame)
  "Convert time FRAME into range of dates."
  (let* ((now (current-time))
         (day (* 60 60 24))
         (fmt "%Y-%m-%d")
         (tomorrow (time-add now day)))
    (pcase frame
      (`this-year (let ((diff (string-to-number (format-time-string "%j" now))))
                    (list
                     (format-time-string fmt (time-subtract now (* day (- diff 1))))
                     (format-time-string fmt tomorrow))))
      (`this-year-full (let ((year (string-to-number (format-time-string "%Y" now))))
                         (list (format "%s-01-01" year) (format "%s-01-01" (+ year 1)))))
      (`this-month (let ((diff (string-to-number (format-time-string "%e" now))))
                     (list
                      (format-time-string fmt (time-subtract now (* day (- diff 1))))
                      (format-time-string fmt tomorrow))))
      (`this-week (let ((diff (string-to-number (format-time-string "%u" now))))
                    (list
                     (format-time-string fmt (time-subtract now (* day (- diff 1))))
                     (format-time-string fmt tomorrow))))
      (`365-days (list
                  (format-time-string fmt (time-subtract now (* day 364)))
                  (format-time-string fmt tomorrow)))
      (`30-days (list
                 (format-time-string fmt (time-subtract now (* day 29)))
                 (format-time-string fmt tomorrow)))
      (`7-days (list
                (format-time-string fmt (time-subtract now (* day 6)))
                (format-time-string fmt tomorrow)))
      (`today (list
               (format-time-string fmt (time-subtract now day))
               :end-incl
               (format-time-string fmt tomorrow)))
      (_ (user-error "Unexpected time frame '%s'" frame)))))



(defun vino-stats-rating-qpr (rating entry)
  "Calculate QPR of RATING based on ENTRY.

QPR calculation is adapted for 10-based float rating and UAH
currency. Ideally I try to achieve the situation where QPR < 1
means bad ratio.

The formula is 100 * (rating / 2)! / price. Usage of factorial
provides a straight-forward way of giving more value to higher
ratings. But rating is divided by 2 to avoid over-valuing max
rating. Result is multiplied by 100 to balance the fact that most
wines in Ukraine cost 100+ UAH.

This formula is a subject for constant change."
  (when-let ((price (car-safe (vino-stats-price entry))))
    (math-div
     (math-mul
      (calcFunc-fact
       (math-div
        (calc-from-number (float (vino-rating-total rating)))
        2))
      100)
     price)))

(defun vino-stats-price (entry)
  "Calculate price of vino ENTRY.

In case entry contains multiple price values, the average it
returned.

This is unfortunate that price is not part of rating. Because
this leads to wrong calculations in some cases. This function
will be revisited once `vino' enforces price as part of rating."
  (when-let ((prices (seq-filter
                      (lambda (p)
                        (and
                         (car p)
                         (string-equal vino-stats-price-currency (cdr p))))
                      (seq-map
                       (lambda (p)
                         (let ((parts (s-split " " p)))
                           (cons (math-read-number (nth 0 parts))
                                 (nth 1 parts))))
                       (vino-entry-price entry)))))
    (cons
     (math-float (calcFunc-rms (apply #'calcFunc-vec (seq-map #'car prices))))
     vino-stats-price-currency)))


;; countries

(defun vino-stats-country-tbl ()
  "Return hash table of region/appellation id to country note.

Keys are ids of regions and appellations. Values are countries
represented as `vulpea-note'.

In order for region/appellation to be part of specific country,
it mast contain a country meta (see `vulpea-meta') linking to
`vulpea-note' tagged as places."
  (let* ((places (vulpea-db-query-by-tags-every '("places")))
         (places-tbl (let ((tbl (make-hash-table
                                 :test 'equal
                                 :size (seq-length places))))
                       (seq-each
                        (lambda (n)
                          (puthash (vulpea-note-id n) n tbl))
                        places)
                       tbl))
         (regions (vulpea-db-query-by-tags-every '("wine" "region")))
         (appellations (vulpea-db-query-by-tags-every '("wine" "appellation")))
         (all (seq-uniq (append regions appellations)
                        (lambda (n1 n2)
                          (string-equal (vulpea-note-id n1)
                                        (vulpea-note-id n2)))))
         (size (seq-length all))
         (result (make-hash-table :test 'equal :size size)))
    (seq-each
     (lambda (n)
       (let ((country (gethash
                       (vulpea-note-meta-get n "country" 'link)
                       places-tbl)))
         (puthash (vulpea-note-id n) country result)))
     all)
    result))



(cl-defstruct vino-stats
  "Various stats based on RATINGS."
  ratings
  price-avg
  price-min
  price-max
  price-total
  rating-min
  rating-max
  rating-avg
  rating-rms
  rating-sdev
  qpr-avg
  qpr-rms)

(defun vino-stats-from-ratings (ratings entries-tbl)
  "Calculate `vino-stats' from RATINGS list based on ENTRIES-TBL.

RATINGS can be related to different entries."
  (let* ((prices (seq-map
                  (lambda (rating)
                    (let ((entry (gethash (vulpea-note-id (vino-rating-wine rating)) entries-tbl)))
                      (vino-stats-price entry)))
                  ratings))
         ;; they can be nil
         (prices (seq-filter #'identity prices))
         ;; we actually need values
         (prices (seq-map #'car prices))
         (prices (when prices (apply #'calcFunc-vec prices)))
         (price-avg (when prices (calcFunc-vmean prices)))
         (price-min (when prices (calcFunc-vmin prices)))
         (price-max (when prices (calcFunc-vmax prices)))
         (price-total (when prices (calcFunc-vsum prices)))
         (totals (seq-map #'vino-rating-total ratings))
         (totals (apply #'calcFunc-vec (seq-map #'calc-from-number totals)))
         (totals-avg (calcFunc-vmean totals))
         (totals-rms (calcFunc-rms totals))
         (totals-sdev (calcFunc-vpvar totals))
         (totals-min (calcFunc-vmin totals))
         (totals-max (calcFunc-vmax totals))
         (qprs (seq-map
                (lambda (rating)
                  (let ((entry (gethash (vulpea-note-id (vino-rating-wine rating)) entries-tbl)))
                    (vino-stats-rating-qpr rating entry)))
                ratings))
         ;; they can be nil
         (qprs (seq-filter #'identity qprs))
         (qprs (when qprs (apply #'calcFunc-vec qprs)))
         (qpr-avg (when qprs (calcFunc-vmean qprs)))
         (qpr-rms (when qprs (calcFunc-rms qprs))))
    (make-vino-stats
     :ratings ratings
     :price-avg price-avg
     :price-min price-min
     :price-max price-max
     :price-total price-total
     :rating-min totals-min
     :rating-max totals-max
     :rating-avg totals-avg
     :rating-rms totals-rms
     :rating-sdev totals-sdev
     :qpr-avg qpr-avg
     :qpr-rms qpr-rms)))


;; grouping

(defun vino-stats-group-ratings-by (ratings-tbl to-key)
  "Group RATINGS-TBL by key produced by TO-KEY.

TO-KEY is a function accepting two arguments - rating id and
rating struct. Result must be of type string or list of strings.

Return hash table where keys are produced by TO-KEY and values
are lists of ratings."
  (let ((tbl (make-hash-table :test 'equal)))
    (maphash
     (lambda (id rating)
       (let* ((keys (funcall to-key id rating))
              (keys (if (listp keys) keys (list keys))))
         (seq-each
          (lambda (key)
            (puthash
             key
             (cons rating (gethash key tbl))
             tbl))
          keys)))
     ratings-tbl)
    tbl))

(defun vino-stats--grouped-ratings-data (ratings-tbl entries-tbl)
  "Return stats for grouped RATINGS-TBL based on ENTRIES-TBL."
  (seq-sort-by
   (lambda (x) (nth 1 x))
   #'>
   (seq-map
    (lambda (key)
      (let* ((ratings (gethash key ratings-tbl))
             (stats (vino-stats-from-ratings ratings entries-tbl)))
        (list key
              (seq-length ratings)
              (vino-stats-format-price (vino-stats-price-total stats))
              (vino-stats-format-price (vino-stats-price-avg stats))
              (vino-stats-format-price (vino-stats-price-min stats))
              (vino-stats-format-price (vino-stats-price-max stats))
              (format "%.2f" (calc-to-number (vino-stats-rating-rms stats)))
              (format "%.4f" (calc-to-number (vino-stats-rating-sdev stats)))
              (format "%.2f" (calc-to-number (vino-stats-rating-min stats)))
              (format "%.2f" (calc-to-number (vino-stats-rating-max stats)))
              (if-let ((qpr (vino-stats-qpr-rms stats)))
                  (format "%.4f" (calc-to-number qpr))
                "NA"))))
    (hash-table-keys ratings-tbl))))


;; data set

(defun vino-stats-data-set-for (range)
  "Read data set for RANGE.

RANGE is a list of two dates in format YYYY-MM-DD: [from, to).

Result is a plist (:range :size :ratings-tbl :entries-tbl :countries-tbl)"
  (when-let*
      ((ratings (seq-map
                 #'car-safe
                 (org-roam-db-query
                  [:select [id]
                   :from ratings
                   :where (and (>= date $s1)
                               (< date $s2))
                   :order-by [(asc date)]]
                  (nth 0 range)
                  (nth 1 range))))
       (size (seq-length ratings))
       (ratings-tbl (let ((tbl (make-hash-table
                                :test 'equal
                                :size size)))
                      (emacsql-with-transaction (org-roam-db)
                        (seq-each
                         (lambda (id)
                           (puthash id (vino-rating-get-by-id id) tbl))
                         ratings))
                      tbl))
       (entries-tbl (let ((tbl (make-hash-table
                                :test 'equal
                                :size size)))
                      (emacsql-with-transaction (org-roam-db)
                        (maphash
                         (lambda (_ rating)
                           (let ((id (vulpea-note-id (vino-rating-wine rating))))
                             (puthash id (vino-entry-get-by-id id) tbl)))
                         ratings-tbl))
                      tbl))
       (countries-tbl (vino-stats-country-tbl)))
    (list :range range
          :size size
          :ratings-tbl ratings-tbl
          :entries-tbl entries-tbl
          :countries-tbl countries-tbl)))

(defun vino-stats-from-data-set (data)
  "Calculate rating stats from DATA set."
  (vino-stats-from-ratings
   (hash-table-values (plist-get data :ratings-tbl))
   (plist-get data :entries-tbl)))

(defun vino-stats-from-data-set-by-country (data)
  "Calculate ratings stats from DATA set grouped by country."
  (let* ((ratings-tbl (plist-get data :ratings-tbl))
         (entries-tbl (plist-get data :entries-tbl))
         (countries-tbl (plist-get data :countries-tbl))
         (tbl (vino-stats-group-ratings-by
               ratings-tbl
               (lambda (_ rating)
                 (let* ((entry (gethash (vulpea-note-id (vino-rating-wine rating))
                                        entries-tbl))
                        (country (gethash (vulpea-note-id
                                           (or (vino-entry-region entry)
                                               (vino-entry-appellation entry)))
                                          countries-tbl)))
                   (vulpea-note-title country))))))
    (vino-stats--grouped-ratings-data tbl entries-tbl)))

(defun vino-stats-from-data-set-by-region (data)
  "Calculate ratings stats from DATA set grouped by rora.

Hey, rora is region or appellation."
  (let* ((ratings-tbl (plist-get data :ratings-tbl))
         (entries-tbl (plist-get data :entries-tbl))
         (tbl (vino-stats-group-ratings-by
               ratings-tbl
               (lambda (_ rating)
                 (let ((entry (gethash (vulpea-note-id (vino-rating-wine rating))
                                       entries-tbl)))
                   (vulpea-note-title
                    (or (vino-entry-region entry)
                        (vino-entry-appellation entry))))))))
    (vino-stats--grouped-ratings-data tbl entries-tbl)))

(defun vino-stats-from-data-set-by-colour (data)
  "Calculate ratings stats from DATA set grouped by colour."
  (let* ((ratings-tbl (plist-get data :ratings-tbl))
         (entries-tbl (plist-get data :entries-tbl))
         (tbl (vino-stats-group-ratings-by
               ratings-tbl
               (lambda (_ rating)
                 (vino-entry-colour
                  (gethash (vulpea-note-id (vino-rating-wine rating))
                           entries-tbl))))))
    (vino-stats--grouped-ratings-data tbl entries-tbl)))

(defun vino-stats-from-data-set-by-carbonation (data)
  "Calculate ratings stats from DATA set grouped by carbonation."
  (let* ((ratings-tbl (plist-get data :ratings-tbl))
         (entries-tbl (plist-get data :entries-tbl))
         (tbl (vino-stats-group-ratings-by
               ratings-tbl
               (lambda (_ rating)
                 (vino-entry-carbonation
                  (gethash (vulpea-note-id (vino-rating-wine rating))
                           entries-tbl))))))
    (vino-stats--grouped-ratings-data tbl entries-tbl)))

(defun vino-stats-from-data-set-by-vintage (data)
  "Calculate ratings stats from DATA set grouped by vintage."
  (let* ((ratings-tbl (plist-get data :ratings-tbl))
         (entries-tbl (plist-get data :entries-tbl))
         (tbl (vino-stats-group-ratings-by
               ratings-tbl
               (lambda (_ rating)
                 (or
                  (vino-entry-vintage
                   (gethash (vulpea-note-id (vino-rating-wine rating))
                            entries-tbl))
                  0)))))
    (-map
     (lambda (row)
       (if (= 0 (car row))
           (cons "NV" (cdr row))
         row))
     (seq-sort-by
      (lambda (x) (nth 0 x))
      #'>
      (vino-stats--grouped-ratings-data tbl entries-tbl)))))

(defun vino-stats-from-data-set-by-grape (data)
  "Calculate ratings stats from DATA set grouped by grape."
  (let* ((ratings-tbl (plist-get data :ratings-tbl))
         (entries-tbl (plist-get data :entries-tbl))
         (tbl (vino-stats-group-ratings-by
               ratings-tbl
               (lambda (_ rating)
                 (seq-map
                  #'vulpea-note-title
                  (vino-entry-grapes
                   (gethash (vulpea-note-id (vino-rating-wine rating))
                            entries-tbl)))))))
    (vino-stats--grouped-ratings-data tbl entries-tbl)))

(defun vino-stats-from-data-set-by-key (data key)
  "Calculate ratings stats from DATA set grouped by KEY.

KEY is one of: country, colour, carbonation, vintage, grape."
  (let* ((stat-fn (pcase key
                    (`country #'vino-stats-from-data-set-by-country)
                    (`colour #'vino-stats-from-data-set-by-colour)
                    (`carbonation #'vino-stats-from-data-set-by-carbonation)
                    (`vintage #'vino-stats-from-data-set-by-vintage)
                    (`grape #'vino-stats-from-data-set-by-grape)
                    (_ (user-error "Unsupported grouping")))))
    (funcall stat-fn data)))


;; interactive functions

;;;###autoload
(defun vino-stats (&optional frame)
  "Display stats for an interactively selected time FRAME."
  (interactive)
  (when-let*
      ((date-min (caar
                  (org-roam-db-query
                   [:select [date]
                    :from ratings
                    :order-by [(asc date)]
                    :limit 1])))
       (date-max (caar
                  (org-roam-db-query
                   [:select [date]
                    :from ratings
                    :order-by [(desc date)]
                    :limit 1])))
       (frame (or
               frame
               (intern
                (completing-read
                 "Time frame: " (cons 'custom vino-stats-time-frames)
                 nil 'require-match))))
       (range (pcase frame
                (`eternity (list date-min date-max))
                (`custom (list
                          (org-read-date nil nil nil "From (inclusive)")
                          (org-read-date nil nil nil "To (exclusive)")))
                (_ (vino-stats--time-frame-range frame))))
       (data (vino-stats-data-set-for range))
       (ratings-tbl (plist-get data :ratings-tbl))
       (entries-tbl (plist-get data :entries-tbl))
       (countries-tbl (plist-get data :countries-tbl))
       (ratings-stat (vino-stats-from-data-set data))
       (countries-stat (vino-stats-from-data-set-by-country data))
       (regions-stat (vino-stats-from-data-set-by-region data))
       (colours-stat (vino-stats-from-data-set-by-colour data))
       (carbonation-stat (vino-stats-from-data-set-by-carbonation data))
       (vintage-stat (vino-stats-from-data-set-by-vintage data))
       (grapes-stat (vino-stats-from-data-set-by-grape data)))
    (buffer-display-result-with "*vino-stats*"
      (format "Stats for period from %s to %s"
              (propertize (nth 0 range) 'face 'bold)
              (propertize (nth 1 range) 'face 'bold))
      ""

      (propertize "General stats" 'face 'bold)
      (string-table
       :row-start "- "
       :sep " : "
       :pad-type '(right left)
       :data
       (list (list "Wines consumed" 0)
             (list "Wines rated" (seq-length (hash-table-keys entries-tbl)))
             (list "Ratings" (seq-length (hash-table-keys ratings-tbl)))
             (list "Countries" (seq-length countries-stat))
             (list "Regions" (seq-length regions-stat))
             (list "Grapes" (seq-length grapes-stat))
             (list "Vintage youngest" (seq-max (seq-remove #'stringp (seq-map (lambda (x) (nth 0 x)) vintage-stat))))
             (list "Vintage oldest" (seq-min (seq-remove #'stringp (seq-map (lambda (x) (nth 0 x)) vintage-stat))))
             (list "Price total" (vino-stats-format-price (vino-stats-price-total ratings-stat)))
             (list "Price avg" (vino-stats-format-price (vino-stats-price-avg ratings-stat)))
             (list "Price min" (vino-stats-format-price (vino-stats-price-min ratings-stat)))
             (list "Price max" (vino-stats-format-price (vino-stats-price-max ratings-stat)))
             (list "Rating avg" (format "%.4f" (calc-to-number (vino-stats-rating-avg ratings-stat))))
             (list "Rating rms" (format "%.4f" (calc-to-number (vino-stats-rating-rms ratings-stat))))
             (list "Rating sdev" (format "%.4f" (calc-to-number (vino-stats-rating-sdev ratings-stat))))
             (list "Rating min" (format "%.4f" (calc-to-number (vino-stats-rating-min ratings-stat))))
             (list "Rating max" (format "%.4f" (calc-to-number (vino-stats-rating-max ratings-stat))))
             (list "QPR avg" (format "%.4f" (calc-to-number (vino-stats-qpr-avg ratings-stat))))
             (list "QPR rms" (format "%.4f" (calc-to-number (vino-stats-qpr-rms ratings-stat))))))
      ""

      (propertize "Country stats" 'face 'bold)
      (string-table
       :header '("country" "count" "p total" "p avg" "p min" "p max" "r rms" "r sdev" "r min" "r max" "qpr")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data countries-stat)
      ""

      (propertize "Vintage stats" 'face 'bold)
      (string-table
       :header '("vintage" "count" "p total" "p avg" "p min" "p max" "r rms" "r sdev" "r min" "r max" "qpr")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data vintage-stat)
      ""

      (propertize "Grape stats" 'face 'bold)
      (string-table
       :header '("grape" "count" "p total" "p avg" "p min" "p max" "r rms" "r sdev" "r min" "r max" "qpr")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data grapes-stat)
      ""

      (propertize "Colour stats" 'face 'bold)
      (string-table
       :header '("country" "count" "p total" "p avg" "p min" "p max" "r rms" "r sdev" "r min" "r max" "qpr")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data colours-stat)
      ""

      (propertize "Carbonation stats" 'face 'bold)
      (string-table
       :header '("country" "count" "p total" "p avg" "p min" "p max" "r rms" "r sdev" "r min" "r max" "qpr")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data carbonation-stat)
      ""

      (propertize "Ratings" 'face 'bold)
      (string-table
       :width '(full full 20 20 24 full 24 full full full full full full)
       :header '("date" "country" "appellation" "producer" "name" "year" "grapes" "color" "carbonation" "sweetness" "price" "rate" "QPR")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :pad-type '(left right right right right left right left left left left left left)
       :data
       (seq-map
        (lambda (id)
          (let* ((rating (gethash id ratings-tbl))
                 (entry (gethash (vulpea-note-id (vino-rating-wine rating)) entries-tbl)))
            (list
             (vino-rating-date rating)
             (gethash (vulpea-note-id
                       (or (vino-entry-region entry)
                           (vino-entry-appellation entry)))
                      countries-tbl)
             (or (vino-entry-region entry)
                 (vino-entry-appellation entry))
             (vino-entry-producer entry)
             (vulpea-buttonize (vino-rating-wine rating)
                               (lambda (_) (vino-entry-name entry)))
             (or (vino-entry-vintage entry) "NV")
             (or (mapconcat #'vulpea-buttonize (vino-entry-grapes entry) ", ") "NA")
             (or (vino-entry-colour entry) "NA")
             (or (vino-entry-carbonation entry) "NA")
             (or (vino-entry-sweetness entry) "NA")
             (vino-stats-format-price (vino-stats-price entry))
             (format "%.2f" (vino-rating-total rating))
             (if-let ((qpr (vino-stats-rating-qpr rating entry)))
                 (format "%.4f" (calc-to-number qpr))
               "NA"))))
        (hash-table-keys ratings-tbl))))))


;; tables for org-mode evaluation

(cl-defun vino-stats-ratings-tbl-for (&key range columns)
  "Return an org-table object of ratings for RANGE.

COLUMNS control which columns to return. Unless specified, all data is returned."
  (let* ((columns (or columns '("date"
                                "country"
                                "appellation"
                                "producer"
                                "name"
                                "vintage"
                                "grapes"
                                "color"
                                "carbonation"
                                "sweetness"
                                "price"
                                "rate"
                                "QPR")))
         (data (vino-stats-data-set-for range))
         (ratings-tbl (plist-get data :ratings-tbl))
         (entries-tbl (plist-get data :entries-tbl))
         (countries-tbl (plist-get data :countries-tbl)))
    (-concat
     (list columns 'hline)
     (-map
      (lambda (id)
        (let* ((rating (gethash id ratings-tbl))
               (entry (gethash (vulpea-note-id (vino-rating-wine rating)) entries-tbl)))
          (-concat
           (when (-contains-p columns "date")
             (list (vino-rating-date rating)))
           (when (-contains-p columns "country")
             (list (vulpea-utils-link-make-string
                    (gethash
                     (vulpea-note-id
                      (or (vino-entry-region entry)
                          (vino-entry-appellation entry)))
                     countries-tbl))))
           (when (-contains-p columns "appellation")
             (list (vulpea-utils-link-make-string
                    (or (vino-entry-region entry)
                        (vino-entry-appellation entry)))))
           (when (-contains-p columns "producer")
             (list (vulpea-utils-link-make-string (vino-entry-producer entry))))
           (when (-contains-p columns "name")
             (list (vulpea-utils-link-make-string (vino-rating-wine rating))))
           (when (-contains-p columns "vintage")
             (list (or (vino-entry-vintage entry) "NV")))
           (when (-contains-p columns "grapes")
             (list (or (mapconcat #'vulpea-utils-link-make-string (vino-entry-grapes entry) ", ") "NA")))
           (when (-contains-p columns "colour")
             (list (or (vino-entry-colour entry) "NA")))
           (when (-contains-p columns "carbonation")
             (list (or (vino-entry-carbonation entry) "NA")))
           (when (-contains-p columns "sweetness")
             (list (or (vino-entry-sweetness entry) "NA")))
           (when (-contains-p columns "price")
             (list (vino-stats-format-price (vino-stats-price entry))))
           (when (-contains-p columns "rate")
             (list (format "%.2f" (vino-rating-total rating))))
           (when (-contains-p columns "QPR")
             (when-let ((qpr (vino-stats-rating-qpr rating entry)))
               (list (format "%.4f" (calc-to-number qpr))))))))
      (hash-table-keys ratings-tbl)))))

(cl-defun vino-stats-grouped-data-tbl-for (key &key range columns)
  "Return an org-table object of grouped data for RANGE.

Grouping is controlled by KEY, which must be supported by
`vino-stats-from-data-set-by'.

COLUMNS control which columns to return. Unless specified, all data is returned."
  (declare (indent 1))
  (let* ((all-columns (list
                       (symbol-name key)
                       "count"
                       "p total"
                       "p avg"
                       "p min"
                       "p max"
                       "r rms"
                       "r sdev"
                       "r min"
                       "r max"
                       "qpr"))
         (columns (or columns all-columns))
         (data (vino-stats-data-set-for range))
         (stats (cons all-columns (vino-stats-from-data-set-by-key data key)))
         (stats-transposed (table-transpose stats)))
    (-concat
     (list columns 'hline)
     (table-transpose
      (-map
       (lambda (key)
         (cdr (--find (string-equal key (car it)) stats-transposed)))
       columns)))))


;; formatting

(defun vino-stats-format-price (price)
  "Format PRICE value."
  (if price
      (let ((value (cond
                    ((null price) nil)
                    ((math-numberp price) (math-float price))
                    ((listp price) (car price))))
            (currency (cond
                       ((math-numberp price) vino-stats-price-currency)
                       ((listp price) (cdr price)))))
        (when value
          (let* ((parts (math-float-parts value t))
                 (a (car parts))
                 (b (car (math-float-parts
                          (math-mul (nth 1 parts) 100) nil))))
            (format "%s.%0.2i %s"
                    (let ((calc-group-char " "))
                      (math-group-float (math-format-number a)))
                    b
                    currency))))
    "NA"))



(provide 'lib-vino-stats)
;;; lib-vino-stats.el ends here
