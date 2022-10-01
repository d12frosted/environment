;;; lib-bg-slides.el --- Utilities for Barberry Garden related presentations  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Sep 2022
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
;; NB! It's a piece of atrocity that helps me. DO NOT REUSE THIS CODE!
;;
;; These helpers are here to ease creation of presentations for
;; Barberry Garden events.
;;
;; See https://barberry.io for more information about the project.
;;
;;; Code:

(require 'lib-brb)
(require 'lib-brb-event)



(defvar bg-slides-dir (expand-file-name "talks-private/" path-projects-dir))



(defvar bg-slides--title)
(defvar bg-slides--subtitle)
(defvar bg-slides--date)

(defun bg-slides-template-title-full ()
  "Return an event title and (optionally) subtitle for slides template."
  (let ((title (or bg-slides--title
                   (let (s)
                     (while (or (not s) (string-empty-p s))
                       (setf s (read-string "Title: ")))
                     s)))
        (subtitle (if bg-slides--title
                      bg-slides--subtitle
                    (read-string "Subtitle: "))))
    (setf bg-slides--title nil)
    (setf bg-slides--subtitle nil)
    (concat
     "#+title: " title
     (unless (string-empty-p subtitle)
       (concat "\n#+subtitle: " subtitle)))))

(defun bg-slides-template-date ()
  "Return an event date for slides template."
  (let ((date (or bg-slides--date (org-read-date nil t))))
    (setf bg-slides--date nil)
    (format-time-string "%B %d, %Y" date)))



(defvar bg-slides--wine-h-regexp "\\* Wine #\\([0-9]+\\)")

(defvar-local bg-slides--wine nil)

(defun bg-slides-wine-cur-idx ()
  "Resolve current wine index (1-based)."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward bg-slides--wine-h-regexp nil t)
        (string-to-number (match-string 1))
      0)))

(defun bg-slides-wine-next-pos ()
  "Resolve next wine position in buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (org-previous-visible-heading 1)
      (point))))

(defun bg-slides-template-wine-name ()
  "Return wine name for slides template."
  (let ((wine bg-slides--wine))
    (vulpea-note-title wine)))

(defun bg-slides-template-wine-info ()
  "Return wine name for slides template."
  (let ((wine bg-slides--wine))
    (brb-wine-info wine 'description 'pick-price)))

(defun bg-slides-template-wine-image ()
  "Copy main image and return it as link for template."
  (let* ((wine bg-slides--wine)
         (image (vulpea-note-meta-get wine "images" 'link))
         (image (string-remove-prefix "attachment:" image))
         (source (expand-file-name image (org-attach-dir-from-id (vulpea-note-id wine))))
         (images-dir (expand-file-name "images/" (file-name-directory (buffer-file-name (current-buffer)))))
         (target (expand-file-name image images-dir)))
    (mkdir images-dir t)
    (shell-command (format "convert '%s' -strip -auto-orient '%s'"
                           source target))
    (format "file:images/%s\n" image)))

(defun bg-slides-insert-wine ()
  "Select and insert a wine into slides."
  (interactive)
  (let ((wine (vulpea-select-from "Wine" (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                  :require-match t))
        (name "Barberry Garden Slides - wine"))
    (setf bg-slides--wine wine)
    (goto-char (bg-slides-wine-next-pos))
    (file-template-insert-by-name name)))



(defun bg-slides-generate ()
  "Generate slides file from event note."
  (interactive)
  (let* ((event (brb-event-select))
         (slug (vulpea-utils-with-note event
                 (vulpea-buffer-prop-get "slug")))
         (wines (brb-event-wines event))
         (dir (expand-file-name slug bg-slides-dir))
         (slides-file (expand-file-name "slides.org" dir))
         (slides-buffer))
    (mkdir dir t)
    (setf slides-buffer (find-file-noselect slides-file))
    (setf bg-slides--title (vulpea-note-title event))
    (setf bg-slides--subtitle (vulpea-utils-with-note event
                                (vulpea-buffer-prop-get "subtitle")))
    (setf bg-slides--date (org-time-string-to-time
                           (vulpea-utils-with-note event
                             (vulpea-buffer-prop-get "date"))))
    (with-current-buffer slides-buffer
      (delete-region (point-min) (point-max))
      (file-template-insert-by-name "Barberry Garden Slides - structure")
      (--each wines
        (setf bg-slides--wine (vulpea-db-get-by-id it))
        (goto-char (bg-slides-wine-next-pos))
        (file-template-insert-by-name "Barberry Garden Slides - wine")))
    (display-buffer slides-buffer)))



(provide 'lib-bg-slides)
;;; lib-bg-slides.el ends here
