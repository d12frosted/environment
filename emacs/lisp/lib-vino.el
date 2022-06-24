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
(require 'vino)

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
(defun vino-balance-display ()
  "Display buffer with balances."
  (interactive)
  (let* ((name "*vino inventory*")
         (buffer (buffer-generate name 'unique))
         (balances (->> (inventory-balance-list vino-inventory-file)
                        (-map
                         (-partial #'-update-at 0
                                   #'vulpea-db-get-by-id))
                        (seq-sort-by
                         (-compose #'vulpea-note-title #'car)
                         #'string<)))
         (total (--reduce-from
                 (+ acc (cdr it))
                 0
                 balances))
         (bottles (--reduce-from
                   (+ acc (max 1 (cdr it)))
                   0
                   balances)))
    (with-current-buffer buffer
      (seq-do
       (lambda (kvp)
         (insert
          (format "%05.2f" (cdr kvp))
          "  "
          (vulpea-utils-link-make-string (car kvp))
          "\n"))
       balances)
      (insert "\n"
              "---"
              "\n"
              "total: *"
              (format "%05.2f" total)
              "* ("
              (format "%i" bottles) " bottles)")
      (org-mode)
      (read-only-mode))
    (display-buffer buffer)))

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
                      "Wine")
                     " | "
                     (org-link-make-string
                      (concat "id:" (vulpea-note-id note))
                      "Rating")
                     "\n\n")
             (seq-do
              (lambda (network)
                (let ((value (vulpea-note-meta-get note network)))
                  (when (or (null value)
                            (string-equal value "false"))
                    (insert
                     (string-pad
                      (concat (capitalize network) ":")
                      (+ 2 (seq-max (seq-map #'length networks)))))
                    (insert-text-button
                     "post"
                     'note note
                     'network network
                     'status "true"
                     'action #'vino-rating-mark-network-action)
                    (insert " / ")
                    (insert-text-button
                     "skip"
                     'note note
                     'network network
                     'status "skip"
                     'action #'vino-rating-mark-network-action)
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

(provide 'lib-vino)
;;; lib-vino.el ends here
