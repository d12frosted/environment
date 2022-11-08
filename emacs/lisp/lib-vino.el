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

(require 'lib-inventory)
(require 'lib-buffer)
(require 'vino)
(require 'request)
(require 'request-deferred)

(defvar vino-inventory-file nil
  "Path to journal file.")

;;;###autoload
(defun vino-availability-get (id)
  "Get availability info for `vino-entry' with ID."
  (cons
   (inventory-total-in vino-inventory-file id)
   (inventory-total-out vino-inventory-file id)))

;;;###autoload
(defun vino-availability-add (id amount source date)
  "Add AMOUNT of `vino-entry' with ID from SOURCE on DATE."
  (inventory-add vino-inventory-file id amount source date))

;;;###autoload
(defun vino-availability-sub (id amount action date)
  "Subtrack AMOUNT of `vino-entry' with ID by ACTION on DATE."
  (inventory-sub vino-inventory-file id amount action date))

;;;###autoload
(defun vino-entry-find-file-available ()
  "Select and visit available `vino-entry'."
  (interactive)
  (let* ((available (seq-map
                     #'car
                     (inventory-balance-list
                      vino-inventory-file)))
         (res (vulpea-select
               "Wine"
               :filter-fn
               (lambda (note)
                 (let ((tags (vulpea-note-tags note)))
                   (and (seq-contains-p tags "wine")
                        (seq-contains-p tags "cellar")
                        (seq-contains-p available
                                        (vulpea-note-id note))))))))
    (if (vulpea-note-id res)
        (find-file (vulpea-note-path res))
      (user-error
       "Can not visit vino entry that does not exist: %s"
       (vulpea-note-title res)))))

;;;###autoload
(defun vino-balance-review ()
  "Review available bottles."
  (interactive)
  (let* ((name "*vino inventory review*")
         (buffer (buffer-generate name 'unique))
         (confirmed (make-hash-table :test 'equal)))
    (with-current-buffer buffer
      (vino-balance-review-refresh buffer confirmed))
    (switch-to-buffer buffer)))

(defun vino-balance-review-refresh (buffer confirmed)
  "Refresh review data in BUFFER.

CONFIRMED is a hashtable with amount of confirmed bottles."
  (let ((cp 0)
        (balances (->> (inventory-balance-list vino-inventory-file)
                       (--map (-update-at 0 #'vulpea-db-get-by-id it))
                       (seq-sort-by
                        (-compose #'vulpea-note-title #'car)
                        #'string<))))
    (with-current-buffer buffer
      (read-only-mode -1)
      (setf cp (point))
      (delete-region (point-min) (point-max))
      (seq-do
       (lambda (kvp)
         (let* ((c (or (gethash (vulpea-note-id (car kvp)) confirmed) 0))
                (b (cdr kvp))
                (vs (concat
                     (format "%05.2f" c)
                     "/"
                     (format "%05.2f" b))))
           (insert
            (buttonize
             "[1]"
             (lambda (&rest _)
               (puthash (vulpea-note-id (car kvp))
                        (+ 1 (or (gethash (vulpea-note-id (car kvp)) confirmed) 0))
                        confirmed)
               (vino-balance-review-refresh buffer confirmed)))
            " "
            (buttonize
             "[all]"
             (lambda (&rest _)
               (puthash (vulpea-note-id (car kvp))
                        (cdr kvp)
                        confirmed)
               (vino-balance-review-refresh buffer confirmed)))
            " "
            (propertize
             (cond
              ((< c b) (propertize vs 'face 'error))
              ((> c b) (propertize vs 'face 'warning))
              (t (propertize vs 'face 'success)))
             'font-lock-ignore t)
            " "
            (string-from (car kvp))
            "\n")))
       balances)
      (insert
       "\n"
       "---"
       "\n"
       (buttonize
        "[refresh]"
        (lambda (&rest _)
          (vino-balance-review-refresh buffer confirmed)))
       " "
       (buttonize
        "[reset]"
        (lambda (&rest _)
          (vino-balance-review-refresh buffer (make-hash-table :test 'equal))))
       "\n"
       "total: *"
       (format "%05.2f" (--reduce-from
                         (+ acc (cdr it))
                         0
                         balances))
       "* ("
       (format "%i" (--reduce-from
                     (+ acc (max 1 (cdr it)))
                     0
                     balances))
       " bottles)")
      (goto-char cp)
      (read-only-mode +1))))

;;;###autoload
(defun vino-latest-ratings-display ()
  "Display buffer with latest ratings."
  (interactive)
  (let* ((name "*vino ratings*")
         (buffer (buffer-generate name 'unique))
         (limit 100)
         (ratings-raw (vino-db-query
                       [:select [id wine date version values]
                        :from ratings
                        :order-by date :desc
                        :limit $s1]
                       limit))
         (ratings (emacsql-with-transaction (vino-db)
                    (seq-map
                     (lambda (row)
                       (cons (vulpea-db-get-by-id (nth 0 row))
                             (make-vino-rating
                              :wine (vulpea-db-get-by-id (nth 1 row))
                              :date (nth 2 row)
                              :version (nth 3 row)
                              :values (nth 4 row))))
                     ratings-raw))))
    (with-current-buffer buffer
      (seq-do
       (lambda (kvp)
         (insert
          "["
          (vino-rating-date (cdr kvp))
          "] ("
          (format "%05.2f" (vino-rating-total (cdr kvp)))
          ") "
          (vulpea-utils-link-make-string (car kvp))
          "\n"))
       ratings)
      (org-mode)
      (read-only-mode)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

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
         (notes (seq-take notes 36))
         (notes (seq-sort-by (lambda (note)
                               (vulpea-note-meta-get note "date"))
                             #'string<
                             notes)))
    (emacsql-with-transaction (vino-db)
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
           (let* ((rating (vino-db-get-rating (vulpea-note-id note)))
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
                  (buffer-substring-no-properties
                   (org-element-property :end pl)
                   (point-max)))))
             (unfill-region pos (point))
             (delete-char -1)
             (insert
              "\n\n"
              "Tasted on " (vino-rating-date rating) "\n"
              "\n")))
         notes)
        (read-only-mode)
        (visual-line-mode)
        (goto-char (point-min))))
    (switch-to-buffer buffer)))

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
    (emacsql-with-transaction (vino-db)
      (with-current-buffer buffer
        (seq-do
         (lambda (note)
           (let ((rating (vino-db-get-rating (vulpea-note-id note))))
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
(defun vino-sources (_)
  "Get the list of vino sources."
  (inventory-sources vino-inventory-file))

;;;###autoload
(defun vino-select-location ()
  "Select a location."
  (vulpea-select "Location"))

;;;###autoload
(defun vino-select-convive ()
  "Select a convive."
  (vulpea-select
   "Convive"
   :filter-fn (lambda (note)
                (seq-contains-p
                 (vulpea-note-tags note)
                 "people"))))

;;;###autoload
(defun vino-list-entries-without-image ()
  "List vino entries without primary image."
  (interactive)
  (let ((buffer (buffer-generate "*wines without images*" 'unique)))
    (with-current-buffer buffer
      (--each
          (->> (vulpea-db-query-by-tags-every '("wine" "cellar" "barberry/public"))
               (--remove (vulpea-note-meta-get-list it "images"))
               (-map #'vulpea-buttonize))
        (insert "- " it "\n"))
      (read-only-mode +1))
    (switch-to-buffer buffer)))



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

(provide 'lib-vino)
;;; lib-vino.el ends here
