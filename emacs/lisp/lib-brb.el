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
(require 'lib-string)

;; * Price

(defvar brb-currency "UAH")

(defun brb-price-format (amount)
  "Format AMOUNT as price."
  (format
   "%s %s"
   (if amount (string-group-number (ceiling amount))
     "–")
   brb-currency))

(defun brb-price-to-number (price)
  "Convert PRICE to number.

Returns nil if PRICE is of different currency than
`brb-currency'."
  (when (s-suffix-p brb-currency price)
    (string-to-number price)))

;; * QPR

(defun brb-qpr (price score &optional volume)
  "Calculate QPR.

SCORE is a rational number in [0, 5].
PRICE is a positive number in `brb-currency'.
VOLUME is a positive number in ml (default 750).

QPR is adjusted to account for VOLUME."
  (when (and score price (> price 0))
    (setq volume (or volume 750))
    (setq price (* price (/ 750.0 volume)))
    (/
     (*
      100
      (calc-to-number (calcFunc-fact (calc-from-number score))))
     (if (= 0 price) 1 price))))

;; * Candidates for deletion

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
           (when-let ((a (vulpea-note-meta-get wine "base")))
             (cons "base" a))
           (when-let ((a (vulpea-note-meta-get wine "bottled")))
             (cons "bottled" a))
           (when-let ((a (vulpea-note-meta-get wine "degorgee")))
             (cons "disgorged" a))
           (when-let ((a (vulpea-note-meta-get wine "sur lie")))
             (cons "sur lie" a))
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
             (t v)))
         (str (if v (format fmt v) "-")))
    (pcase style
      (`normal str)
      (`bold (concat "*" str "*"))
      (`del (concat "+" str "+")))))

(cl-defun brb-format-float-in (v &key floats fn style prec)
  "Format float V with precision PREC.

When both FLOATS and FN are provided, FN is called with FLOATS
and if V equals to result, then it's styled using STYLE."
  (if v
      (let* ((floats (-filter #'identity floats))
             (b (and floats fn (funcall fn floats))))
        (brb-format-float v
          :prec prec
          :style (if (and b (= v b)) style 'normal)))
    "-"))

;; * Social links

(cl-defun brb-link-exists (url)
  "Return URL if it exists."
  (let* ((cmd (concat "curl -o /dev/null -LIsw '%{http_code}\n' '" url "'"))
         (status (s-trim (shell-command-to-string cmd))))
    (when (string-equal "200" status)
      url)))

(cl-defun brb-sabotage-link (wine-bureau-id)
  "Return sabotage link for WINE-BUREAU-ID (if it exists)."
  (let ((url (concat "https://sabotage.wine/product/" (s-downcase wine-bureau-id))))
    (brb-link-exists url)))

(cl-defun brb-vivino-link (vivino-id vintage)
  "Return vivino link for VIVINO-ID and VINTAGE if it exists."
  (let ((url (concat "https://www.vivino.com/w/"
                     vivino-id
                     (if vintage (format "?year=%s" vintage) ""))))
    (brb-link-exists url)))

;; * External data synchronisation (social links and prices)

(defun brb-sync-external-data-with-upstream ()
  "Sync external data with upstream."
  (interactive)
  ;; make sure that barberry/public is set on all notes that require it
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-level 0)
                                   (--filter
                                    (and (not (vulpea-note-tagged-all-p it "barberry/public"))
                                         (or (vulpea-note-tagged-all-p it "wine" "grape")
                                             (vulpea-note-tagged-all-p it "wine" "region")
                                             (vulpea-note-tagged-all-p it "wine" "appellation")
                                             (vulpea-note-tagged-all-p it "places")))))
    (vulpea-buffer-tags-add "barberry/public"))

  ;; make sure that people I drink with are marked as public convives
  (vulpea-utils-process-notes (->> (-union
                                    ;; convives from ratings
                                    (--mapcat (vulpea-note-meta-get-list it "convive" 'link)
                                              (vulpea-db-query-by-tags-every '("wine" "rating")))
                                    ;; participants of events
                                    (--mapcat (-union (vulpea-note-meta-get-list it "participants" 'link)
                                                      (vulpea-note-meta-get-list it "waiting" 'link))
                                              (vulpea-db-query-by-tags-every '("wine" "event"))))
                                   (-uniq)
                                   (vulpea-db-query-by-ids)
                                   (--remove (vulpea-note-tagged-all-p it "barberry/public" "barberry/convive")))
    (vulpea-buffer-tags-add "barberry/public")
    (vulpea-buffer-tags-add "barberry/convive"))

  ;; update sabotage links in wine entries
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   (--filter (vulpea-note-meta-get it "externalId")))
    (if-let ((url (brb-sabotage-link (vulpea-note-meta-get it "externalId"))))
        (vulpea-buffer-meta-set "sabotage" url 'append)
      (vulpea-buffer-meta-remove "sabotage")))

  ;; update vivino links in wine entries
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   (--filter (vulpea-note-meta-get it "vivinoId")))
    (if-let ((url (brb-vivino-link (vulpea-note-meta-get it "vivinoId")
                                   (vulpea-note-meta-get it "vintage"))))
        (vulpea-buffer-meta-set "vivino" url 'append)
      (vulpea-buffer-meta-remove "vivino")))

  ;; update goodwine links in wine entries
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   (--filter (vulpea-note-meta-get it "goodwine")))
    (unless (brb-link-exists (vulpea-note-meta-get it "goodwine" 'link))
      (vulpea-buffer-meta-remove "sabotage")))

  ;; update goodwine links in wine entries
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   (--filter (vulpea-note-meta-get it "winewine")))
    (unless (brb-link-exists (vulpea-note-meta-get it "winewine" 'link))
      (vulpea-buffer-meta-remove "winewine"))))

(provide 'lib-brb)
;;; lib-brb.el ends here
