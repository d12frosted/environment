;;; lib-brb.el --- Various utilities for Barberry Garden stats -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Jun 2022
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

(require 'dash)
(require 'vulpea)
(require 'lib-calc)
(require 'lib-table)



(defvar brb-currency "UAH")

(defun brb-price-format (amount)
  "Format AMOUNT as price."
  (format "%d %s" amount brb-currency))

(defun brb-price-to-number (price)
  "Convert PRICE to number.

Returns nil if PRICE is of different currency than
`brb-currency'."
  (when (s-suffix-p brb-currency price)
    (string-to-number price)))



(defun brb-wine-info (wine list-mode &optional price-mode)
  "Return info about WINE note.

Used for slides and blog posts.

LIST-MODE is either description or regular.

When WINE entry has multiple price records, only one price is
returned, but the value depends on PRICE-MODE:

- max-price returns the highest;
- min-price returns the smallest;
- avg-price returns the average;
- pick-price interactively asks user to select.

In all cases, except for interactive, only price entries with
`brb-currency' are taken into consideration."
  (let ((sep (pcase list-mode
               (`description " :: ")
               (_ ": "))))
    (string-join
     (->> (list
           (cons "producer"
                 (vulpea-utils-link-make-string
                  (vulpea-note-meta-get wine "producer" 'note)))
           (cons "name"
                 (org-link-make-string
                  (concat "id:" (vulpea-note-id wine))
                  (vulpea-note-meta-get wine "name")))
           (cons "vintage"
                 (or (vulpea-note-meta-get wine "vintage") "NV"))
           (cons "grapes"
                 (string-join
                  (-map #'vulpea-utils-link-make-string
                        (vulpea-note-meta-get-list wine "grapes" 'note))
                  ", "))
           (when-let ((a (vulpea-note-meta-get wine "appellation" 'note)))
             (cons "appellation" (vulpea-utils-link-make-string a)))
           (when-let ((a (vulpea-note-meta-get wine "region" 'note)))
             (cons "region" (vulpea-utils-link-make-string a)))
           (cons "location"
                 (let* ((a (or (vulpea-note-meta-get wine "appellation" 'note)
                               (vulpea-note-meta-get wine "region" 'note)))
                        (b a)
                        (res nil)
                        (go t))
                   (while go
                     (setq b (or (vulpea-note-meta-get b "parent" 'note)
                                 (vulpea-note-meta-get b "region" 'note)))
                     (unless b
                       (setq go nil
                             b (vulpea-note-meta-get a "country" 'note)))
                     (setq res (cons (vulpea-utils-link-make-string b) res)))
                   (string-join (seq-reverse res) ", ")))
           (cons "alcohol" (vulpea-note-meta-get wine "alcohol"))
           (cons "sugar" (or (vulpea-note-meta-get wine "sugar") "N/A"))
           (cons "price" (let ((prices (vulpea-note-meta-get-list wine "price")))
                           (if (= 1 (seq-length prices))
                               (car prices)
                             (pcase price-mode
                               (`pick-price (completing-read
                                             (concat "Price (" (vulpea-note-title wine) "): ")
                                             prices nil t))
                               (_ (concat (->> prices
                                               (--filter (s-suffix-p brb-currency it))
                                               (-map #'string-to-number)
                                               (apply #'calcFunc-vec)
                                               (funcall
                                                (pcase price-mode
                                                  (`max-price #'calcFunc-vmax)
                                                  (`min-price #'calcFunc-vmin)
                                                  (`avg-price #'calcFunc-vmean)))
                                               (calc-to-number)
                                               (floor)
                                               (number-to-string))
                                          " UAH")))))))
          (-filter #'identity)
          (--map (concat "- " (car it) sep (cdr it))))
     "\n")))



(defun brb-position-by (row pred)
  "Find first position in ROW satisfying PRED.

Position is 1-based, while index is 0-based."
  (when-let ((idx (-find-index pred row)))
    (1+ idx)))

(defun brb-position-of (row str)
  "Find first position of STR in ROW .

Position is 1-based, while index is 0-based."
  (brb-position-by row (-partial #'string-equal str)))

(defun brb-positions-by (row pred)
  "Find all positions in ROW satisfying PRED.

Position is 1-based, while index is 0-based."
  (when-let ((idxs (-find-indices pred row)))
    (-map #'1+ idxs)))

(defun brb-positions-of (row str)
  "Find all positions of STR in ROW .

Position is 1-based, while index is 0-based."
  (brb-positions-by row (-partial #'string-equal str)))



(cl-defun brb-format-float (v &key style prec)
  "Format float V with precision PREC.

STYLE is either bold, del or normal (default)."
  (declare (indent 1))
  (let* ((style (or style 'normal))
         (prec (or prec 2))
         (fmt (concat "%." (number-to-string prec) "f"))
         (v (cond
             ((and (stringp v) (string-empty-p v)) nil)
             ((stringp v) (string-to-number v))
             (t v))))
    (if v
        (pcase style
          (`normal (format fmt v))
          (`bold (concat "*" (format fmt v) "*"))
          (`del (concat "+" (format fmt v) "+")))
      "-")))

(cl-defun brb-format-float-in (v &key floats fn style prec)
  "Format float V with precision PREC.

When both FLOATS and FN are provided, FN is called with FLOATS
and if V equals to result, then it's styled using STYLE."
  (if v
      (let ((b (and floats fn (funcall fn floats))))
        (brb-format-float v
          :prec prec
          :style (if (and b (= v b)) style 'normal)))
    ""))



(defun brb-raw-scores-to-people (tbl)
  "Convert raw scores to individual scores.

TBL represents raw scores."
  (let* ((wines (-drop 2 (car tbl)))
         (people (->> tbl (-map 'car) (-remove 'string-empty-p)))
         (ratings (-filter #'identity (-map #'identity (table-select-rows "rating" tbl :column 1))))
         (favourites (-map (-rpartial #'brb-positions-of "favourite") (table-select-rows "extremum" tbl :column 1)))
         (outcasts (-map (-rpartial #'brb-positions-of "outcast") (table-select-rows "extremum" tbl :column 1))))
    (-concat
     (list
      (cons " " wines)
      'hline)
     (-map-indexed
      (lambda (i p)
        (let ((rs (nth i ratings)))
          (cons p (-map-indexed
                   (lambda (ri r)
                     (brb-format-float r
                       :style
                       (cond
                        ((-contains-p (nth i favourites) (1+ ri)) 'bold)
                        ((-contains-p (nth i outcasts) (1+ ri)) 'del)
                        (t 'normal))))
                   rs))))
      people))))

(cl-defun brb-raw-scores-to-summary (tbl &key columns)
  "Convert raw scores to summary.

TBL represents raw scores.

When COLUMNS is not specified, all columns are returned.
Otherwise only those specified in the list."
  (let* ((wines (- (length (car tbl)) 2))
         (names (-drop 2 (car tbl)))
         (ratings (-filter #'identity (-map #'identity (table-select-rows "rating" tbl :column 1))))
         (prices (car (table-select-rows "price" tbl :column 1)))

         (totals (table-vreduce-columns #'calcFunc-vsum ratings))
         (amean (table-vreduce-columns #'calcFunc-vmean ratings))
         (rmss (table-vreduce-columns (lambda (&rest vecs) (calcFunc-rms (apply #'calcFunc-vec vecs))) ratings))
         (sdevs (table-vreduce-columns #'calcFunc-vpvar ratings))
         (favourites (-filter #'identity
                              (-map (-rpartial #'brb-positions-of "favourite")
                                    (table-select-rows "extremum" tbl :column 1))))
         (outcasts (-filter #'identity
                            (-map (-rpartial #'brb-positions-of "outcast")
                                  (table-select-rows "extremum" tbl :column 1))))
         (favourited (-map (lambda (i) (-count (-rpartial #'-contains-p i) favourites))
                           (-iota wines 1)))
         (outcasted (-map (lambda (i) (-count (-rpartial #'-contains-p i) outcasts))
                          (-iota wines 1)))

         (qprs (-map (lambda (i)
                       (/
                        (*
                         100
                         (calc-to-number (calcFunc-fact (calc-from-number (nth i amean)))))
                        (if (= 0 (nth i prices))
                            1
                          (nth i prices))))
                     (-iota wines)))
         (columns (or columns '("total" "amean" "rms" "sdev" "favourite" "outcast" "price" "QPR"))))
    (-concat
     (list (cons "" columns)
           'hline)
     (-map
      (lambda (i)
        (-filter
         #'identity
         (list
          (nth i names)
          (when (-contains-p columns "total")
            (brb-format-float-in (nth i totals) :floats totals :fn #'-max :style 'bold))
          (when (-contains-p columns "amean")
            (brb-format-float-in (nth i amean) :floats amean :fn #'-max :style 'bold :prec 4))
          (when (-contains-p columns "rms")
            (brb-format-float-in (nth i rmss) :floats rmss :fn #'-max :style 'bold :prec 4))
          (when (-contains-p columns "sdev")
            (brb-format-float-in (nth i sdevs) :prec 4))
          (when (-contains-p columns "favourite")
            (brb-format-float-in (nth i favourited) :floats favourited :fn #'-max :style 'bold))
          (when (-contains-p columns "outcast")
            (brb-format-float-in (nth i outcasted) :floats outcasted :fn #'-max :style 'del))
          (when (-contains-p columns "price")
            (brb-format-float-in (nth i prices)))
          (when (-contains-p columns "QPR")
            (brb-format-float-in (nth i qprs) :floats qprs :fn #'-max :style 'bold :prec 4)))))
      (-iota wines)))))



(provide 'lib-brb)
;;; lib-brb.el ends here
