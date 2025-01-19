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
(require 'brb)
(require 'lib-calc)
(require 'lib-table)
(require 'lib-string)

(defun brb-qpr-data ()
  "Test data for QPR experiments."
  (with-temp-buffer
    (let ((data (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                     (--filter (vulpea-note-meta-get it "price"))
                     (--filter (s-suffix-p "UAH" (vulpea-note-meta-get it "price")))
                     (--filter (vulpea-note-meta-get it "rating"))
                     (--filter (vulpea-note-tagged-all-p it "barberry/public"))
                     (-filter (lambda (wine)
                                (--filter
                                 (string> (vulpea-note-meta-get it "date") "2024-01-01")
                                 (vulpea-note-meta-get-list wine "ratings" 'note))))
                     (--map `(("producer" . ,(vulpea-note-title (vulpea-note-meta-get it "producer" 'note)))
                              ("name" . ,(vulpea-note-meta-get it "name"))
                              ("wine" . ,(vulpea-note-title it))
                              ("price" . ,(vulpea-note-meta-get it "price" 'number))
                              ("rating" . ,(vulpea-note-meta-get it "rating" 'number))
                              ("appellation" . ,(string-match-n
                                                 3
                                                 org-link-any-re
                                                 (or (vulpea-note-meta-get it "appellation") "")))
                              ("qpr" . ,(brb-qpr
                                         (vulpea-note-meta-get it "price" 'number)
                                         (vulpea-note-meta-get it "rating" 'number)
                                         it))
                              ("qpr1" . ,(brb-qpr-1
                                         (vulpea-note-meta-get it "price" 'number)
                                         (vulpea-note-meta-get it "rating" 'number)))
                              ("qpr2" . ,(brb-qpr-2
                                         (vulpea-note-meta-get it "price" 'number)
                                         (vulpea-note-meta-get it "rating" 'number))))))))
      (let ((json-encoding-pretty-print t))
        (insert (json-encode data)))
      (kill-new (buffer-substring-no-properties (point-min) (point-max))))))

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

;; * Prices

(cl-defun brb-sabotage-price (wine-bureau-id)
  "Return price for WINE-BUREAU-ID."
  (when-let* ((url (brb-sabotage-link wine-bureau-id))
              (cmd (concat "curl -sL '" url "' | hq '{wine: .col-md-7 | {title: .product-title-page, price: .andro_product-price}}' | jq -r '.wine.price'"))
              (cmd (concat "curl -sL '" url "' | hq '{wine: {title: .product-title-page, price: .buy-row-price-text}}' | jq -r '.wine.price'"))
              (raw (s-replace " " "" (s-trim (shell-command-to-string cmd))))
              (price (string-to-number raw)))
    (when (> price 0)
      (round price))))

(cl-defun brb-winewine-price (url)
  "Return price of wine from WineWine URL."
  (when-let* ((url url)
              (cmd (concat "curl -sL '" url "' | hq '{price: .woocommerce-Price-amount}' | jq -r '.price'"))
              (raw (s-trim (shell-command-to-string cmd)))
              (price (string-to-number raw)))
    (when (> price 0)
      (round price))))

(cl-defun brb--add-price (note price)
  "Add PRICE to wine NOTE in current buffer."
  (if-let ((priceNew price)
           (priceOld (or (vulpea-note-meta-get note "price" 'number) 0)))
      (if (= priceNew priceOld)
          (message "[%s] price has not changed: %d" (vulpea-note-title note) priceNew)
        (message "[%s] price changed: %d -> %d"
                 (vulpea-note-title note)
                 priceOld
                 priceNew)
        (unless (= 0 priceOld)
          (vulpea-buffer-meta-set "price private"
                                  (-uniq
                                   (cons (vulpea-note-meta-get note "price")
                                         (vulpea-note-meta-get-list note "price private")))))
        (vulpea-buffer-meta-set "price" (format "%d %s" priceNew brb-currency))
        (vulpea-buffer-meta-set "price date" (format-time-string "%F"))
        (vulpea-buffer-meta-sort vino-entry-meta-props-order))
    (message "[%s] could not extract price"
             (vulpea-note-title note))))

;; * External data synchronisation (social links and prices)

;;;###autoload
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
                                   (--filter (vulpea-note-meta-get it "wineBureauId")))
    (if-let ((url (brb-sabotage-link (vulpea-note-meta-get it "wineBureauId"))))
        (vulpea-buffer-meta-set "sabotage" url 'append)
      (vulpea-buffer-meta-remove "sabotage")))

  ;; update prices from sabotage
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   (--filter (vulpea-note-meta-get it "sabotage")))
    (brb--add-price it (brb-sabotage-price (vulpea-note-meta-get it "wineBureauId"))))

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

  ;; update winewine links in wine entries
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   (--filter (vulpea-note-meta-get it "winewine")))
    (unless (brb-link-exists (vulpea-note-meta-get it "winewine" 'link))
      (vulpea-buffer-meta-remove "winewine")))

  ;; update prices from winewine
  (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   (--filter (vulpea-note-meta-get it "winewine")))
    (brb--add-price it (brb-winewine-price (vulpea-note-meta-get it "winewine" 'link)))))

(provide 'lib-brb)
;;; lib-brb.el ends here
