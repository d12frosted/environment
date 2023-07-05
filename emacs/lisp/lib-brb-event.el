;;; lib-brb-event.el --- Barberry Garden event-related helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Sep 2022
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

(require 'org-ml)
(require 'vulpea)
(require 'lib-vino-stats)
(require 'lib-brb-ledger)



(defvar brb-event-narrator-id "bc8aa837-3348-45e6-8468-85510966527a")



;;;###autoload
(defun brb-event-select ()
  "Interactively select an event note."
  (let* ((tags '("wine" "event"))
         (event (when (and (eq major-mode 'org-mode)
                           (buffer-file-name))
                  (when-let* ((id (save-excursion
                                    (goto-char (point-min))
                                    (org-id-get)))
                              (note (vulpea-db-get-by-id id)))
                    (when (--every-p (-contains-p (vulpea-note-tags note) it) tags)
                      note)))))
    (vulpea-select-from
     "Event"
     (--filter
      (= 0 (vulpea-note-level it))
      (vulpea-db-query-by-tags-every tags))
     :require-match t
     :initial-prompt (when event (vulpea-note-title event)))))

;;;###autoload
(defun brb-events-from-range (range)
  "Return list of events in time RANGE."
  (->> (vulpea-db-query-by-tags-every '("wine" "event" "barberry/public"))
       (--filter (= 0 (vulpea-note-level it)))
       (--filter (let ((date (vulpea-utils-with-note it
                               (vulpea-buffer-prop-get "date"))))
                   (and (org-time>= date (nth 0 range))
                        (org-time< date (nth 1 range)))))
       (--sort (org-time< (vulpea-utils-with-note it
                            (vulpea-buffer-prop-get "date"))
                          (vulpea-utils-with-note other
                            (vulpea-buffer-prop-get "date"))))))



;;;###autoload
(defun brb-event-wines (event)
  "Return list of wines from EVENT."
  (vulpea-utils-with-note event
    (let ((bound (save-excursion
                   (goto-char (point-min))
                   (re-search-forward org-heading-regexp)
                   (beginning-of-line)
                   (point))))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "1. " bound 'no-error)
          (beginning-of-line)
          (->> (org-ml-parse-element-at (point))
               (org-ml-get-children)
               (-map #'org-ml-item-get-paragraph)
               (-map #'car)
               (--map (org-ml-get-property :path it))
               (-map #'vulpea-db-get-by-id)))))))

(defun brb-event-wines--prices (event)
  "Return prices of wines from EVENT.

Result is a properly list (:public :real), where each value is a
list of prices (from the first to the last wine)."
  (vulpea-utils-with-note event
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "#\\+name: data")
      (re-search-forward "^|")
      (goto-char (line-beginning-position))
      (let* ((raw (->> (org-table-to-lisp)
                       (--remove (eq 'hline it))
                       (-map #'cdr)
                       (--map (--map (substring-no-properties it) it))
                       (--filter (seq-contains-p '("price" "price real") (car it)))))
             (public (--find (string-equal "price" (car it)) raw))
             (real (--find (string-equal "price real" (car it)) raw)))
        (list
         :public (->> public (cdr) (-map #'string-to-number))
         :real (->> real (cdr) (-map #'string-to-number)))))))



;;;###autoload
(defun brb-event-participants (event)
  "Return list of participants from EVENT."
  (vulpea-utils-with-note event
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (format org-complex-heading-regexp-format (regexp-quote "Preparation")))
      (search-forward "1. ")
      (beginning-of-line)
      (->> (org-ml-parse-element-at (point))
           (org-ml-get-children)
           (-map #'org-ml-item-get-paragraph)
           (-map #'car)
           (--map (org-ml-get-property :path it))
           (vulpea-db-query-by-ids)
           (--remove (vulpea-note-primary-title it))))))



;;;###autoload
(defun brb-event-score-summary (event)
  "Return score summary of EVENT."
  (vulpea-utils-with-note event
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "#\\+results: summary")
      (forward-line 1)
      (let* ((raw (->> (org-table-to-lisp)
                       (--remove (eq 'hline it))
                       (--map (--map (->> it
                                          (substring-no-properties)
                                          (s-replace-regexp "[*+]" ""))
                                     it))))
             (headline (car raw)))
        (--map (--zip-with (cons it (unless (or (string-empty-p other)
                                                (string-equal "-" other))
                                      (string-to-number other)))
                           (cdr headline)
                           (cdr it))
               (cdr raw))))))

;;;###autoload
(defun brb-event-score-personal (event)
  "Return personal scores of EVENT."
  (let* ((tbl (vulpea-utils-with-note event
                (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "#\\+name: data")
                  (re-search-forward "|")
                  (->> (org-table-to-lisp)
                       (--remove (eq 'hline it))))))
         (wines (-drop 2 (car tbl)))
         (people (->> tbl (-map 'car) (-remove 'string-empty-p) (-map #'substring-no-properties)))
         (ratings (-filter #'identity (-map #'identity (table-select-rows "rating" tbl :column 1))))
         (favourites (-map (-rpartial #'brb-positions-of '("favourite" "fav" "+"))
                           (table-select-rows "extremum" tbl :column 1)))
         (outcasts (-map (-rpartial #'brb-positions-of '("outcast" "out" "-"))
                         (table-select-rows "extremum" tbl :column 1))))
    (--map-indexed
     (let ((rs (nth it-index ratings)))
       (list
        :convive
        (cond
         ((string-match string-uuid-regexp it) (match-string 1 it))
         (t it))
        :ratings
        (--map
         (cond
          ((and (stringp it) (string-empty-p it)) nil)
          ((stringp it) (string-to-number it))
          (t it))
         rs)
        :favourites (nth it-index favourites)
        :outcasts (nth it-index outcasts)))
     people)))



(provide 'lib-brb-event)
;;; lib-brb-event.el ends here
