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
(require 'lib-brb)
(require 'lib-string)
(require 'vino)
(require 'vino-inv)
(require 'request)
(require 'request-deferred)

;; * insert methods

(defun vino-insert-region-candidates (&optional filter)
  "Return list of candidates for `vulpea-find'.

FILTER is a `vulpea-note' predicate."
  (let ((notes (vulpea-db-query-by-tags-every '("wine" "region"))))
    (if filter
        (-filter filter notes)
      notes)))

(defun vino-insert-appellation-candidates (&optional filter)
  "Return list of candidates for `vulpea-find'.

FILTER is a `vulpea-note' predicate."
  (let ((notes (vulpea-db-query-by-tags-every '("wine" "appellation"))))
    (if filter
        (-filter filter notes)
      notes)))

;;;###autoload
(defun vino-insert-region ()
  "Select a region and insert a link to it."
  (interactive)
  (let ((vulpea-insert-default-candidates-source #'vino-insert-region-candidates))
    (funcall-interactively
     #'vulpea-insert
     nil
     (lambda (title props)
       (vino-region-create :title title :capture-properties props)))))

;;;###autoload
(defun vino-insert-appellation ()
  "Select a appellation and insert a link to it."
  (interactive)
  (let ((vulpea-insert-default-candidates-source #'vino-insert-appellation-candidates))
    (funcall-interactively
     #'vulpea-insert
     nil
     (lambda (title props)
       (vino-appellation-create :title title :capture-properties props)))))

;; * vino hooks

;;;###autoload
(defun vino-entry-assign-social-links (&optional note)
  "Interactively assign extra meta for wine NOTE."
  (interactive)
  (let* ((note (vino-entry-note-get-dwim note))
         (wineBureauId (s-presence (read-string "Wine Bureau ID: "
                                                (vulpea-note-meta-get note "wineBureauId"))))
         (sabotage (when wineBureauId (brb-sabotage-link wineBureauId)))
         (winewineId (unless wineBureauId
                       (s-presence (read-string "Wine Wine ID: "
                                                (vulpea-note-meta-get note "winewineId")))))
         (winewine (when winewineId (or
                                     (vulpea-note-meta-get note "winewine" 'link)
                                     (s-presence (read-string "Wine Wine URL: ")))))
         (vivinoUrlRaw)
         (vivinoId)
         (vivino))

    (kill-new (vulpea-note-title note))
    (setq vivinoUrlRaw (or
                        (vulpea-note-meta-get note "vivino" 'link)
                        (s-presence (read-string "Vivino URL: "))))
    (while vivinoUrlRaw
      (setq vivinoId (s-presence (string-match-1 "^https://www.vivino.com.*/w/\\([0-9]+\\).*$" vivinoUrlRaw)))
      (if vivinoId
          (setq vivinoUrlRaw nil)
        (message "Could not extract vivino ID")
        (sit-for 1)
        (setq vivinoUrlRaw (s-presence (read-string "Vivino URL: ")))))
    (setq vivino (brb-vivino-link vivinoId (vulpea-note-meta-get note "vintage")))

    (vulpea-utils-with-note note
      (when wineBureauId
        (vulpea-buffer-meta-set "wineBureauId" wineBureauId))
      (when sabotage
        (vulpea-buffer-meta-set "sabotage" sabotage))
      (when winewineId
        (vulpea-buffer-meta-set "winewineId" winewineId))
      (when winewine
        (vulpea-buffer-meta-set "winewine" winewine))
      (when vivinoId
        (vulpea-buffer-meta-set "vivinoId" vivinoId))
      (when vivino
        (vulpea-buffer-meta-set "vivino" vivino))
      (vulpea-buffer-meta-sort vino-entry-meta-props-order))))

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
      (vulpea-buffer-meta-set "bottle" bottle-id 'append))

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
                                (vulpea-select-multiple-from
                                 "Convive"
                                 (vulpea-db-query-by-tags-every '("people"))
                                 :require-match t)
                                'append)))))

;; * vino-inv hooks

;;;###autoload
(defun vino-inv-acquire-bottle-handler (bottle)
  "Handle wine BOTTLE acquire event."
  (let* ((wine (vino-inv-bottle-wine bottle))
         (price (vino-inv-bottle-price bottle))
         (price (cond
                 ((s-suffix-p brb-currency price) (string-to-number price))
                 ((= 0 (string-to-number price)) 0)
                 (t (read-number (format "Convert %s to UAH: " price))))))
    (unless (= 0 price)
      (vulpea-utils-with-note wine
        (vulpea-buffer-meta-set "price date" (format-time-string "%F"))
        (vulpea-buffer-meta-sort vino-entry-meta-props-order)
        (save-buffer))

      (brb-ledger-record-txn
       :date (date-to-time (vino-inv-bottle-purchase-date bottle))
       :comment (concat "[" (vulpea-note-id wine) "]")
       :account-to "spending:wines"
       :account-from "personal:account"
       :amount price))))

;;;###autoload
(defun vino-inv-consume-bottle-handler (bottle action date)
  "Handle wine BOTTLE consume event via ACTION on DATE."
  (when (string-equal "sell" action)
    (let* ((wine (vino-inv-bottle-wine bottle))
           (price (vino-inv-bottle-price bottle))
           (price (read-number "Sell for: "
                               (when (s-suffix-p brb-currency price)
                                 (string-to-number price)))))
      (unless (= 0 price)
        (brb-ledger-record-txn
         :date date
         :comment (concat "[" (vulpea-note-id wine) "]")
         :account-to "personal:account"
         :account-from "income:sell"
         :amount price)))))

;; * vino-inv extensions

;;;###autoload
(defun vino-inv-ui-print-info ()
  "Save print info for the marked bottles."
  (interactive)
  (let (print-list bottle)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (when (eq cmd ?\*)
          ;; This is the key PKG-DESC.
          (setq bottle (tabulated-list-get-id))
          (push bottle print-list))
        (forward-line)))
    (when (seq-empty-p print-list)
      (user-error "There are no marked bottles."))
    (let* ((data (->> print-list
                      (--map
                       (let ((bottle-id (string-to-number (nth 1 (s-split ":" it)))))
                         (vino-inv-get-bottle bottle-id)))
                      (--sort (< (vino-inv-bottle-id it)
                                 (vino-inv-bottle-id other)))
                      (--map
                       (list (format "#%s" (vino-inv-bottle-id it))
                             (vino-inv-bottle-purchase-date it)
                             (concat "https://barberry.io/wines/" (vulpea-note-id (vino-inv-bottle-wine it)))))))
           (file (make-temp-file "vino-inv-" nil ".csv"))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        (insert (string-join '("bottle" "date" "url") ",") "\n")
        (--each data
          (insert (string-join it ",") "\n")))
      (switch-to-buffer buffer)
      (when (y-or-n-p "Convert?")
        (save-buffer)
        (shell-command-to-string
         (format "ssconvert '%s' '%s'"
                 file
                 (expand-file-name
                  "Documents/bottles.xlsx"
                  path-home-dir)))))))

;;;###autoload
(defun vino-inv-ui-mark ()
  "Mark entry at point."
  (interactive)
  (if-let ((tagged (save-excursion
                     (beginning-of-line)
                     (eq (char-after) ?\*))))
      (tabulated-list-put-tag " " t)
    (tabulated-list-put-tag "*" t)))

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
  (let* ((networks '("vivino"))
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
         (notes (--sort
                 (let ((date1 (vulpea-note-meta-get it "date"))
                       (date2 (vulpea-note-meta-get other "date")))
                   (or (string< date1 date2)
                       (< (or (vulpea-note-meta-get it "order" 'number) 0)
                          (or (vulpea-note-meta-get other "order" 'number) 0))))
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
                  (pos)
                  (has-meta))
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
              (format "%i"
                      (let* ((s5 (vino-rating-total rating))
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
             (when-let ((event (vulpea-note-meta-get note "event" 'note))
                        (order (vulpea-note-meta-get note "order" 'number)))
               (setq has-meta t)
               (insert "Wine #" (number-to-string order) " on " (vulpea-note-title event) "\n"))
             (when-let ((volume (vulpea-note-meta-get (vino-rating-wine rating) "volume" 'number)))
               (unless (= 750 volume)
                 (setq has-meta t)
                 (pcase volume
                   (`1500 (insert "Magnum bottle\n"))
                   (`375 (insert "Half bottle\n")))))
             (when-let ((x (vulpea-note-meta-get (vino-rating-wine rating) "degorgee")))
               (setq has-meta t)
               (insert "Disgorged "
                       (if-let ((time (ignore-errors (org-parse-time-string x))))
                           (concat "on " (format-time-string "%F" (encode-time time)))
                         (concat "in " x))
                       "\n"))
             (when-let ((x (vulpea-note-meta-get (vino-rating-wine rating) "sur lie")))
               (unless (string-equal "N/A" x)
                 (setq has-meta t)
                 (insert x " on lees\n")))
             (when has-meta
               (insert "\n"))
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
             (insert ""
                     (vino-rating-date rating)
                     " @"
                     (when-let ((loc (vulpea-note-meta-get note "location" 'note)))
                       (or (vulpea-note-meta-get loc "public name")
                           (vulpea-note-title loc)))
                     "\n")
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

;; * Smart region setting

(defconst vino/germany/wein "b4afc600-1592-4830-8402-77961dbc595d")
(defconst vino/germany/qualitatswein "82861689-1a6c-47e9-90c2-71835759f92c")
(defconst vino/germany/pradikatswein "29744fde-ad06-4085-8490-00991be1947b")
(defconst vino/germany/landwein
  '("b893b7ea-1888-4a7a-97ef-e837cff30df9"
    "dc8c4787-73e3-4ffc-9f4d-393d49142cd8"
    "f59b5ff2-098b-42c3-86ca-8e08641cdf94"
    "15793fde-91b7-4a7a-bdcc-7c15fe864ce7"
    "c07c9851-1ed6-4391-b2ca-08cdb4a8d7aa"
    "27830cd0-ac57-4273-8d48-c253ce6c3dd5"
    "a497a039-de97-40d8-8930-de60f70091f1"
    "6c53e3e6-9758-4065-adfc-ed049278f82d"
    "394903af-7980-4727-b76a-49dc334f195a"
    "918ea808-4f7e-44ce-b0d3-939ac6c17297"
    "93deceaf-85fe-46e5-9901-6a8dd92b4e4e"
    "501260a7-8f55-4c6e-947f-ea832f463fcb"
    "347ccf73-a124-4223-a6b7-b09ff397ce87"
    "3949561f-7881-44ff-bb8f-10ed73573397"
    "563e96aa-c8d2-4952-9fd2-22779e5234c5"
    "8733af09-7135-4589-a56e-039dab3ba7b1"
    "7a8348dd-999d-4b35-865a-9ad9de3095a1"
    "eaaf74a6-f9e4-4e1c-87c5-353d9270421f"
    "9e5353a5-c2da-4bb2-bc5d-ec09954d3c27"
    "dddd3512-94c5-4225-b2db-7f84bb360632"
    "9356a819-e53e-48c0-a408-fbc38a7fd8ed"
    "cef1f2c5-2a04-423e-b14f-7892aaf77264"
    "c79491b2-824f-4de8-b4d4-f89824a9c856"
    "76ca5b68-eb8b-45c4-9826-6e4561142ff7"
    "ae668fd4-4221-4f35-8f20-42ad6c421af7"
    "190d3a9b-1fef-4344-a460-88f1b99c9ebc"))
(defconst vino/germany/regions
  '("d132a2b6-5dda-4741-8a37-a918852b0b50"
    "cb214156-1394-4fd1-bd3a-f6036f78b3d1"
    "3025012e-f6c3-40b7-a6dd-931bb8274daa"
    "f01f1c70-005c-4a4c-bf9b-11f97357894e"
    "28aed8eb-0567-42a1-8885-f3098d3b3cd4"
    "a5051493-9ef5-43a6-a9c5-4a7f8865ce1d"
    "c12ca7ee-0a81-4ca7-9222-db77d8a3b6ca"
    "a7607fef-e2c2-4df8-8e49-da195f82a14c"
    "19ca3ef7-5b84-41f5-be47-3bcfbc41a25d"
    "d7edf304-d970-4440-bad1-6a759662e851"
    "aad73bdf-1f85-4d4c-b149-223688c9f748"
    "4108c59d-cc0b-4335-905d-486c12d34c5b"
    "6f0c9592-67c4-436e-a26d-8cbd43504c51"))

;; (setq vino-origin-select-fn #'vino-origin-select-custom)
;;;###autoload
(defun vino-origin-select-custom ()
  "Custom origin selection function.

This one handles some countries in a very custom/specific way.

See `vino-origin-select-fn' for more information."
  (interactive)
  (let ((country (vulpea-select-from
                  "Country"
                  (vulpea-db-query-by-tags-every '("wine" "country"))))
        rora region appellation subregion)
    (unless (vulpea-note-id country)
      (when (y-or-n-p "Country %s doesn not exist. Would you like to create it?")
        (setq country (vino-country-create (vulpea-note-title country)))))
    (when (vulpea-note-id country)
      (pcase (vulpea-note-title country)
        ("Germany"
         (pcase (completing-read "Quality level: " '("wein" "landwein" "qualitätswein" "prädikatswein") nil t)
           ("wein"
            (setq appellation (vulpea-db-get-by-id vino/germany/wein)))
           ("landwein"
            (setq appellation (vulpea-select-from
                               "Landwein"
                               (vulpea-db-query-by-ids vino/germany/landwein)
                               :require-match t))
            (setq region (vulpea-note-meta-get appellation "parent" 'note)))
           ("qualitätswein"
            (setq appellation (vulpea-db-get-by-id vino/germany/qualitatswein))
            (setq region (vulpea-select-from
                          "Region"
                          (vulpea-db-query-by-ids vino/germany/regions)
                          :require-match t)))
           ("prädikatswein"
            (setq appellation (vulpea-db-get-by-id vino/germany/pradikatswein))
            (setq region (vulpea-select-from
                          "Region"
                          (vulpea-db-query-by-ids vino/germany/regions)
                          :require-match t)))))

        ;; default case
        (_ (setq rora (vulpea-select-from
                       "Region or appellation"
                       (->> (append (vulpea-db-query-by-tags-every '("wine" "region"))
                                    (vulpea-db-query-by-tags-every '("wine" "appellation")))
                            (--filter (string-equal (vulpea-note-id country)
                                                    (vulpea-note-meta-get it "country" 'link)))
                            (--remove (vulpea-note-tagged-any-p it "subregion")))))
           (unless (vulpea-note-id rora)
             (setq rora (pcase (completing-read
                                (format "%s does not exist. What to do?"
                                        (vulpea-note-title rora))
                                '("Create region"
                                  "Create appellation"
                                  "Abort"))
                          (`"Create region"
                           (vino-region-create :title (vulpea-note-title rora)
                                               :country country))
                          (`"Create appellation"
                           (vino-appellation-create :title (vulpea-note-title rora)
                                                    :country))
                          (_ (error "Abort")))))
           (if (vulpea-note-tagged-any-p rora "region")
               (setq region rora)
             (setq appellation rora))))

      ;; custom rules

      ;; -> champagne - pick subregion
      (when (and appellation (string-equal "Champagne AOC" (vulpea-note-title appellation)))
        (let ((subregions (vulpea-db-query-by-tags-every '("wine" "champagne" "subregion"))))
          (setq subregion (vino--repeat-while
                             #'vulpea-select-from
                             #'null
                             "Subregion (C-g for none)" subregions :require-match t))))

      (-filter
       #'cdr
       `(("country" . ,country)
         ("region" . ,region)
         ("appellation" . ,appellation)
         ("subregion" . ,subregion))))))

(defun vino-set-region-full ()
  "Set region for vino entry in current buffer."
  (interactive)
  (let ((country (vulpea-select-from
                  "Country"
                  (vulpea-db-query-by-tags-every '("wine" "country"))
                  :require-match t))
        region appellation)
    (pcase (vulpea-note-title country)
      ("Germany"
       (pcase (completing-read "Quality level: " '("wein" "landwein" "qualitätswein" "prädikatswein") nil t)
         ("wein"
          (setq appellation (vulpea-db-get-by-id vino/germany/wein)))
         ("landwein"
          (setq appellation (vulpea-select-from
                             "Landwein"
                             (vulpea-db-query-by-ids vino/germany/landwein)
                             :require-match t))
          (setq region (vulpea-note-meta-get appellation "parent" 'note)))
         ("qualitätswein"
          (setq appellation (vulpea-db-get-by-id vino/germany/qualitatswein))
          (setq region (vulpea-select-from
                        "Region"
                        (vulpea-db-query-by-ids vino/germany/regions)
                        :require-match t)))
         ("prädikatswein"
          (setq appellation (vulpea-db-get-by-id vino/germany/pradikatswein))
          (setq region (vulpea-select-from
                        "Region"
                        (vulpea-db-query-by-ids vino/germany/regions)
                        :require-match t)))))

      ;; default mode
      (_ (let ((note (vulpea-select-from
                      "Region"
                      (->> (vulpea-db-query-by-links-some `(("id" . ,(vulpea-note-id country))))
                           (--filter (and
                                      (or (vulpea-note-tagged-all-p it "wine" "region")
                                          (vulpea-note-tagged-all-p it "wine" "appellation"))
                                      (string-equal (vulpea-meta-get it "country" 'link) (vulpea-note-id country))))))))
           (unless (vulpea-note-id note)
             (setq note
                   (pcase (completing-read
                           (format "Region %s does not exist. What to do?"
                                   (vulpea-note-title note))
                           '("Create region"
                             "Create appellation"
                             "Ignore"))
                     (`"Create region"
                      (vulpea-create
                       (vulpea-note-title note)
                       (concat "wine/region/"
                               (or (vulpea-note-meta-get country "short name")
                                   (vulpea-note-title country))
                               "/%<%Y%m%d%H%M%S>-${slug}.org")
                       :tags (seq-union (plist-get vino-region-template :tags)
                                        '("wine" "region"))
                       :head (plist-get vino-region-template :head)
                       :body (format "- country :: %s" (vulpea-utils-link-make-string country))
                       :context (plist-get vino-region-template :context)
                       :properties (plist-get vino-region-template :properties)
                       :unnarrowed t
                       :immediate-finish t))
                     (`"Create appellation"
                      (vulpea-create
                       (vulpea-note-title note)
                       (concat "wine/appellation/"
                               (or (vulpea-note-meta-get country "short name")
                                   (vulpea-note-title country))
                               "/%<%Y%m%d%H%M%S>-${slug}.org")
                       :tags (seq-union (plist-get vino-appellation-template :tags)
                                        '("wine" "appellation"))
                       :head (plist-get vino-appellation-template :head)
                       :body (format "- country :: %s" (vulpea-utils-link-make-string country))
                       :context (plist-get vino-appellation-template :context)
                       :properties (plist-get vino-appellation-template :properties)
                       :unnarrowed t
                       :immediate-finish t))
                     (_ (user-error "Abort")))))
           (if (vulpea-note-tagged-all-p note "wine" "region")
               (setq region note)
             (setq appellation note)
             (setq region (vulpea-note-meta-get note "parent" 'note))))))
    
    (vulpea-buffer-meta-set "country" country)
    (if appellation
        (vulpea-buffer-meta-set "appellation" appellation)
      (vulpea-buffer-meta-remove "appellation"))
    (if region
        (vulpea-buffer-meta-set "region" region)
      (vulpea-buffer-meta-remove "region"))
    (vulpea-buffer-sort-meta vino-meta-props-order)
    (save-buffer)))

;;;###autoload
(defun vino-entry-origin (wine)
  "Return origin of WINE denoted as `vulpea-note'.

Origin is a list from the widest origin to the most specific. Each
element is a list itself, because some appellations are part of multiple
wine-making regions thus there is a need to support this case. But in
most cases each of these sub-lists should contain single element.

Assumptions:

- Each wine entry can have zero or one appellation.

- Each wine entry can have zero, one or many regions.

- Each appellation/region can have zero, one or many
  parents (appellation or region). The parent is denoted by 'parent'
  meta.

- Each entry must have exactly one country."
  (let* ((country (or (vulpea-note-meta-get wine "country" 'note)
                      (error "Country is missing in '%s' (%s)"
                             (vulpea-note-title wine)
                             (vulpea-note-id wine))))
         (appellation (vulpea-note-meta-get wine "appellation" 'note))
         (regions (vulpea-note-meta-get-list wine "region" 'note))
         (subregion (vulpea-note-meta-get wine "subregion" 'note))
         result
         cursor)
    ;; include subregion tree when present
    (when subregion
      (setq result (cons (list subregion) result))
      (while (and (setq subregion (vulpea-note-meta-get subregion "parent" 'note))
                  (vulpea-note-tagged-all-p subregion "subregion"))
        (setq result (cons (list subregion) result))))

    ;; pick a cursor for parents lookup
    (setq cursor (vulpea-note-meta-get-list
                  (if appellation appellation (nth 0 regions))
                  "parent" 'note))

    ;; include appellation when present
    (when appellation
      (setq result (cons (list appellation) result)))

    ;; include regions when present, but ignore when cursor already
    ;; includes at least one of the regions
    (when (and regions (not (--any-p (-contains-p regions it) cursor)))
      (setq result (cons regions result)))

    ;; traverse parents, but ignore wide trees
    (while (= 1 (seq-length cursor))
      (setq result (cons cursor result))
      (setq cursor (vulpea-note-meta-get-list (nth 0 cursor) "parent" 'note)))

    ;; include country
    (setq result (cons (list country) result))

    ;; done
    result))

(provide 'lib-vino)
;;; lib-vino.el ends here
