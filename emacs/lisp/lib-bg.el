;;; lib-bg.el --- Various utilities to run Barberry Garden  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 11 Sep 2022
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

(defvar bg-currency "UAH"
  "Default currency.")

(defun bg-wine-info (wine list-mode &optional price-mode)
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
`bg-currency' are taken into consideration."
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
                                               (--filter (s-suffix-p bg-currency it))
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
                                          " UAH"))))))
           )
          (-filter #'identity)
          (--map (concat "- " (car it) sep (cdr it))))
     "\n")))

(provide 'lib-bg)
;;; lib-bg.el ends here
