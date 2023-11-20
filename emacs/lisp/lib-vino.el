;;; lib-vino.el --- Vino utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 13 Feb 2021
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
;; Various helpers and utilities extending `vino' module. They are
;; here because there is no place for them in the upstream package.
;;
;;; Code:

(require 'lib-buffer)
(require 'vino)
(require 'vino-inv)
(require 'request)
(require 'request-deferred)

;; * vino hooks

;;;###autoload
(defun vino-rating-assign-extra-meta (rating extra-data)
  "Assign extra meta for RATING note.

EXTRA-DATA contains bottle-id."
  (let* ((wine (vulpea-note-meta-get rating "wine" 'note))
         (bottle-id (assoc-default 'bottle-id extra-data))
         (bottle (vino-inv-get-bottle bottle-id))
         (location (vulpea-select-from "Location"
                                       (vulpea-db-query-by-tags-some '("places" "people" "event"))
                                       :require-match t)))
    (vulpea-utils-with-note rating
      (vulpea-buffer-meta-set "bottle" bottle-id 'append)
      (vulpea-buffer-meta-set "volume" (vino-inv-bottle-volume bottle) 'append))
    (if (vulpea-note-tagged-all-p location "wine" "event")
        (vulpea-utils-with-note rating
          (vulpea-buffer-meta-set "location" (or (vulpea-note-meta-get location "location" 'note) location) 'append)
          (vulpea-buffer-meta-set "event" location 'append)
          (vulpea-buffer-meta-set "order"
                                  (->> (brb-event-wines location)
                                       (--map-indexed (cons it-index it))
                                       (--find (string-equal (vulpea-note-id wine)
                                                             (vulpea-note-id (cdr it))))
                                       (car)
                                       (+ 1))
                                  'append)
          (vulpea-buffer-meta-set "convive"
                                  (--remove (string-equal brb-event-narrator-id (vulpea-note-id it))
                                            (brb-event-participants location))
                                  'append))
      (vulpea-utils-with-note rating
        (vulpea-buffer-meta-set "location" location 'append)
        (vulpea-buffer-meta-set "convive"
                                (let ((people (vulpea-db-query-by-tags-every '("people"))))
                                  (vulpea-utils-collect-while #'vulpea-select-from nil "Convive" people))
                                'append)))

    ;; record transfer between personal and brb accounts
    (when (vulpea-note-tagged-all-p location "wine" "event" "barberry/public")
      (let* ((date (vulpea-note-meta-get rating "date"))
             (price (vino-inv-bottle-price bottle))
             (price (cond
                     ((s-suffix-p brb-currency price) (string-to-number price))
                     ((= 0 (string-to-number price)) 0)
                     (t (read-number (format "Convert %s to UAH: " price))))))
        (brb-ledger-record-txn
         :date (date-to-time date)
         :comment (concat "[" (vulpea-note-id wine) "]")
         :account-to "personal:account"
         :account-from "income:barberry-garden"
         :amount price)))))

;; * vino-inv hooks

(defun vino-inv-acquire-bottle-handler (bottle wine)
  "Handle WINE BOTTLE acquire event."
  (let* ((price (vino-inv-bottle-price bottle))
         (price (cond
                 ((s-suffix-p brb-currency price) (string-to-number price))
                 ((= 0 (string-to-number price)) 0)
                 (t (read-number (format "Convert %s to UAH: " price))))))
    (unless (= 0 price)
      (brb-ledger-record-txn
       :date (date-to-time (vino-inv-bottle-purchase-date bottle))
       :comment (concat "[" (vulpea-note-id wine) "]")
       :account-to "spending:wines"
       :account-from "personal:account"
       :amount price))))

;; * vino-inv extensions

;;;###autoload
(defun vino-inv-ui-print-info ()
  "Display print info for bottle at point."
  (interactive)
  (let* ((bottle-id (vino-inv-ui-get-bottle-id))
         (bottle (vino-inv-get-bottle bottle-id))
         (buffer (get-buffer-create "*vino inventory print info*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert
       "https://barberry.io/wines/" (vulpea-note-id (vino-inv-bottle-wine bottle)) "\n"
       "#" (number-to-string (vino-inv-bottle-id bottle)) "\n"
       (vino-inv-bottle-purchase-date bottle)
       "\n"))
    (display-buffer buffer)))

;;;###autoload
(defun vino-inv-ui-kill-url ()
  "Put URL to the bottle at point into `kill-ring'."
  (interactive)
  (let* ((bottle-id (vino-inv-ui-get-bottle-id))
         (bottle (vino-inv-get-bottle bottle-id))
         (url (concat "https://barberry.io/wines/" (vulpea-note-id (vino-inv-bottle-wine bottle)))))
    (kill-new url)))

;;;###autoload
(defun vino-inv-ui-kill-wine-id ()
  "Put ID of the wine at point into `kill-ring'."
  (interactive)
  (let* ((bottle-id (vino-inv-ui-get-bottle-id))
         (bottle (vino-inv-get-bottle bottle-id)))
    (kill-new (vulpea-note-id (vino-inv-bottle-wine bottle)))))

(defun vino-inv-ui-record-spending ()
  "Record spending of the wine at point."
  (interactive)
  (when (y-or-n-p "Record? ")
    (let* ((bottle-id (vino-inv-ui-get-bottle-id))
           (bottle (vino-inv-get-bottle bottle-id))
           (date (vino-inv-bottle-purchase-date bottle))
           (price (vino-inv-bottle-price bottle))
           (price (if (s-suffix-p brb-currency price)
                      (string-to-number price)
                    (read-number (format "Convert %s to UAH: " price)))))
      (brb-ledger-record-txn
       :date (date-to-time date)
       :comment (concat "[" (vulpea-note-id (vino-inv-bottle-wine bottle)) "]")
       :account-to "spending:wines"
       :account-from "personal:account"
       :amount price)
      (when (get-buffer brb-ledger-buffer-name)
        (brb-ledger-buffer-create)))))

;; * network posting

;;;###autoload
(defun vino-rating-mark-network-action (button)
  "Mark rating note with some status in some network.

BUTTON should be a proper button with following properties:

- note - a `vulpea-note' object
- network - string
- status - string"
  (let ((note (button-get button 'note))
        (network (button-get button 'network))
        (status (button-get button 'status)))
    (vulpea-meta-set note network status 'append)
    (vulpea-utils-with-note note
      (save-buffer))))

;;;###autoload
(defun vino-display-network-candidates ()
  "Display ratings for posting on various networks."
  (interactive)
  (let* ((networks '("vivino" "delectable"))
         (network (if (= 1 (seq-length networks))
                      (car networks)
                    (completing-read "Network: " networks)))
         (name (concat "*" network "*"))
         (buffer (buffer-generate name 'unique))
         (notes (vulpea-db-query-by-tags-every '("wine" "rating")))
         (notes (seq-filter
                 (lambda (note)
                   (let ((v (vulpea-note-meta-get note network)))
                     (or (null v)
                         (string-equal v "false"))))
                 notes))
         (total (seq-length notes))
         (notes (seq-sort-by (lambda (note)
                               (vulpea-note-meta-get note "date"))
                             #'string>
                             notes))
         (notes (seq-take notes 64))
         (notes (seq-sort-by (lambda (note)
                               (vulpea-note-meta-get note "date"))
                             #'string<
                             notes)))
    (emacsql-with-transaction (org-roam-db)
      (with-current-buffer buffer
        (org-mode)
        (insert "Showing "
                (number-to-string (seq-length notes))
                " ratings out of "
                (number-to-string total)
                "\n"
                "\n")
        (seq-do
         (lambda (note)
           (let* ((rating (vino-rating-get-by-id note))
                  (pos))
             (insert
              "* "
              (vulpea-note-title (vino-rating-wine rating))
              "\n\n")
             (insert (org-link-make-string
                      (concat
                       "id:" (vulpea-note-id
                              (vino-rating-wine rating)))
                      "[ Wine ]")
                     " | "
                     (org-link-make-string
                      (concat "id:" (vulpea-note-id note))
                      "[ Rating ]")
                     "\n\n")
             (seq-do
              (lambda (network)
                (let ((value (vulpea-note-meta-get note network)))
                  (when (or (null value)
                            (string-equal value "false"))
                    (insert-text-button
                     "[ post ]"
                     'note note
                     'network network
                     'status "true"
                     'action #'vino-rating-mark-network-action)
                    (insert " / ")
                    (insert-text-button
                     "[ skip ]"
                     'note note
                     'network network
                     'status "skip"
                     'action #'vino-rating-mark-network-action)
                    (insert " " (capitalize network))
                    (insert "\n"))))
              networks)
             (insert "\n")
             (insert
              "Total: "
              (format "%05.2f" (vino-rating-total rating))
              " / "
              (format "%03.1f" (/ (vino-rating-total rating) 2))
              " / "
              (format "%i"
                      (let* ((s5 (/ (vino-rating-total rating) 2))
                             (a (pcase (floor s5)
                                  (`5 100)
                                  (`4 90)
                                  (`3 80)
                                  (`2 70)
                                  (`1 60)
                                  (`0 50)))
                             (b (* 10 (- s5 (floor s5)))))
                        (round (+ a b))))
              "\n\n")
             (setq pos (point))
             (insert
              (vulpea-utils-with-note note
                (let* ((meta (vulpea-buffer-meta))
                       (pl (plist-get meta :pl)))
                  (s-replace-regexp
                   org-link-any-re
                   (lambda (txt) (match-string 3 txt))
                   (buffer-substring-no-properties
                    (org-element-property :end pl)
                    (point-max))))))
             (unfill-region pos (point))
             (delete-char -1)
             (insert "\n\n")
             (when-let (x (vulpea-note-meta-get (vino-rating-wine rating) "degorgee"))
               (insert "Disgorged "
                       (if-let ((time (ignore-errors (org-parse-time-string x))))
                           (concat "on " (format-time-string "%F" (encode-time time)))
                         (concat "in " x))
                       "\n"))
             (when-let (x (vulpea-note-meta-get (vino-rating-wine rating) "sur lie"))
               (insert x " on lees\n\n"))
             (insert "Tasted on " (vino-rating-date rating) "\n")
             (insert "\n")))
         notes)
        (read-only-mode)
        (visual-line-mode)
        (goto-char (point-min))))
    (switch-to-buffer buffer)))

;; * VIVC info

(defun vino-grape-fetch-vivc-info (id)
  "Fetch grape information from VIVC by ID.

Return deferred object associated with grape information
represented as association list."
  (deferred:$
   (request-deferred
    "https://www.vivc.de/index.php"
    :params `(("r" . "passport/view") ("id" . ,id))
    :parser (lambda () (libxml-parse-html-region (point) (point-max))))
   (deferred:nextc
    it
    (lambda (response)
      (let* ((data (request-response-data response))
             (info (->> (dom-by-tag (dom-by-id data "^w1$") 'tr)
                        (--filter
                         (-contains-p
                          '("Prime name"
                            "Color of berry skin"
                            "Country or region of origin of the variety"
                            "Prime name of parent 1"
                            "Prime name of parent 2")
                          (car (dom-strings (dom-by-tag it 'th)))))
                        (--map
                         (cons
                          (s-trim (car (dom-strings (dom-by-tag it 'th))))
                          (s-titleized-words (s-trim (car (dom-strings (dom-by-tag it 'td)))))))))
             (synonyms (->> (dom-by-tag (dom-by-id data "^w7$") 'td)
                            (-map #'dom-strings)
                            (-filter #'identity)
                            (--map (s-titleized-words (s-trim (car it)))))))
        (append info `(("synonyms" . ,synonyms)
                       ("url" . ,(concat "https://www.vivc.de/index.php?r=passport%2Fview&id=" id)))))))))

;;;###autoload
(defun vino-grape-update-vivc-info ()
  "Update information about grape from VIVC."
  (interactive)
  (let* ((tags '("wine" "grape"))
         (vivc-id-prop "vivc-id")
         (grape (when (eq major-mode 'org-mode)
                  (let* ((id (save-excursion
                               (goto-char (point-min))
                               (org-id-get)))
                         (note (vulpea-db-get-by-id id)))
                    (when (--every-p (-contains-p (vulpea-note-tags note) it) tags)
                      note))))
         (grape (vulpea-select-from
                 "Grape" (vulpea-db-query-by-tags-every tags)
                 :require-match t
                 :initial-prompt (when grape (vulpea-note-title grape))))
         (vivc-id (vulpea-note-meta-get grape vivc-id-prop)))
    (unless vivc-id
      (when (y-or-n-p "VIVC ID is not set, would you like to search database?")
        (browse-url
         (concat
          "https://www.vivc.de/index.php?"
          "r=cultivarname%2Findex&"
          "CultivarnameSearch%5Bcultivarnames%5D="
          "&CultivarnameSearch%5Bcultivarnames%5D=cultivarn&"
          "CultivarnameSearch%5Btext%5D=" (vulpea-note-title grape))))
      (setq vivc-id (read-string "VIVC ID: "))
      (vulpea-meta-set grape vivc-id-prop vivc-id))
    (deferred:$
     (vino-grape-fetch-vivc-info vivc-id)
     (deferred:nextc
      it
      (lambda (info)
        (let* ((primary-name (assoc-default "Primary name" info))
               (color (assoc-default "Color of berry skin" info))
               (origin (assoc-default "Country or region of origin of the variety" info))
               (origin (or (--find (string-equal (s-downcase (vulpea-note-title it)) (s-downcase origin))
                                   (vulpea-db-query-by-tags-every '("places")))
                           (user-error "Could not find place '%s'" origin)))
               (grapes (--remove
                        (string-equal (vulpea-note-id it)
                                      (vulpea-note-id grape))
                        (vulpea-db-query-by-tags-every '("wine" "grape"))))
               (parent1 (assoc-default "Prime name of parent 1" info nil "NA"))
               (parent1 (or (--find (string-equal (s-downcase (vulpea-note-title it)) (s-downcase parent1)) grapes)
                            (if (string-empty-p parent1) "NA" parent1)))
               (parent2 (assoc-default "Prime name of parent 2" info nil "NA"))
               (parent2 (or (--find (string-equal (s-downcase (vulpea-note-title it)) (s-downcase parent2)) grapes)
                            (if (string-empty-p parent2) "NA" parent2)))
               (synonyms (assoc-default "synonyms" info))
               (url (assoc-default "url" info))
               (resources (vulpea-note-meta-get-list grape "resources")))
          (unless (--any-p (s-contains-p url it) resources)
            (vulpea-meta-set grape "resources" (cons (format "[[%s][vivc.de]]" url) resources)))
          (vulpea-meta-set grape "origin" origin 'append)
          (vulpea-meta-set grape "parent 1" parent1 'append)
          (vulpea-meta-set grape "parent 2" parent2 'append)
          (vulpea-meta-set grape "colour of skin" (s-downcase color) 'append)
          (--each synonyms
            (when-let ((other (--filter (-intersection (cons (vulpea-note-title it)
                                                             (vulpea-note-aliases it))
                                                       (cons primary-name
                                                             synonyms))
                                        grapes)))
              (--each other
                (message "- %s" (vulpea-note-title it)))
              (user-error "Found duplicate, see messages buffer for more information")))))))))

;; * incomplete ratings

;;;###autoload
(defun vino-display-incomplete-ratings ()
  "Display a buffer listing incomplete rating notes.

Whatever that means."
  (interactive)
  (let* ((name "*vino-incomplete*")
         (buffer (buffer-generate name 'unique))
         (props '("convive" "location"))
         (limit 100)
         (notes (vulpea-db-query
                 (lambda (note)
                   (let ((tags (vulpea-note-tags note))
                         (vs (seq-map (lambda (p)
                                        (vulpea-note-meta-get note p))
                                      props)))
                     (and
                      (seq-contains-p tags "wine")
                      (seq-contains-p tags "rating")
                      (seq-some #'null vs))))))
         (notes (seq-sort-by (lambda (note)
                               (vulpea-note-meta-get note "date"))
                             #'string>
                             notes))
         (notes (seq-take notes limit)))
    (emacsql-with-transaction (org-roam-db)
      (with-current-buffer buffer
        (seq-do
         (lambda (note)
           (let ((rating (vino-rating-get-by-id note)))
             (insert
              "["
              (vino-rating-date rating)
              "] "
              (vulpea-utils-link-make-string note)
              " - missing "
              (string-join
               (seq-filter
                (lambda (p)
                  (null (vulpea-note-meta-get note p)))
                props)
               ", ")
              "\n")))
         notes)
        (org-mode)
        (read-only-mode)
        (goto-char (point-min))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun vino-list-entries-without-image ()
  "List vino entries without primary image."
  (interactive)
  (let ((buffer (buffer-generate "*wines without images*" 'unique)))
    (with-current-buffer buffer
      (--each
          (->> (vulpea-db-query-by-tags-every '("wine" "cellar" "barberry/public"))
               (--remove (vulpea-note-meta-get-list it "images"))
               (--remove (eq 'yes (vulpea-note-meta-get it "image missing" 'symbol)))
               (-map #'vulpea-buttonize))
        (insert "- " it "\n"))
      (read-only-mode +1))
    (switch-to-buffer buffer)))

;;;###autoload
(defun vino-attach-image ()
  "Attach currently viewed image to corresponding vino note."
  (interactive)
  (cond
   ((eq major-mode 'org-mode) (org-open-file (org-attach-dir-get-create)))
   ((eq major-mode 'image-mode)
    (unless (s-prefix-p org-attach-id-dir buffer-file-name)
      (user-error "Seems like this image is not an attachment of some vulpea note"))
    (let* ((parts (s-split "/" (s-chop-prefix org-attach-id-dir buffer-file-name)))
           (id (concat (nth 0 parts) (nth 1 parts)))
           (note (vulpea-db-get-by-id id)))
      (unless note
        (user-error "Could not find note with id %s" id))
      (vulpea-utils-with-note note
        (vulpea-buffer-meta-set "images"  (concat "[[attachment:" (nth 2 parts) "]]") 'append)
        (save-buffer))
      (vulpea-visit note)
      note))
   (t (user-error "Not sure how to help you in %s" major-mode))))

(provide 'lib-vino)
;;; lib-vino.el ends here
