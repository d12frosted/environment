;;; lib-bg-slides.el --- helpers for Barberry Garden slides  -*- lexical-binding: t; -*-
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
;;; Code:



(defun bg-slides-template-title-full ()
  "Return an event title and (optionally) subtitle for slides template."
  (let ((title (let (s)
                 (while (or (not s) (string-empty-p s))
                   (setf s (read-string "Title: ")))
                 s))
        (subtitle (read-string "Subtitle: ")))
    (concat
     "#+title: " title
     (unless (string-empty-p subtitle)
       (concat "\n#+subtitle: " subtitle)))))

(defun bg-slides-template-date ()
  "Return an event date for slides template."
  (let ((date (org-read-date nil t)))
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
    (concat
     "- producer :: "
     (vulpea-utils-link-make-string (vulpea-note-meta-get wine "producer" 'note))
     "\n"

     "- name :: "
     (org-link-make-string
      (concat "id:" (vulpea-note-id wine))
      (vulpea-note-meta-get wine "name"))
     "\n"

     "- vintage :: " (or (vulpea-note-meta-get wine "vintage") "NV") "\n"

     "- grapes :: " (string-join
                     (-map #'vulpea-utils-link-make-string
                           (vulpea-note-meta-get-list wine "grapes" 'note))
                     ", ")
     "\n"

     (if-let ((a (vulpea-note-meta-get wine "appellation" 'note)))
         (concat "- appellation :: "
                 (vulpea-utils-link-make-string a))
       "")
     (if-let ((a (vulpea-note-meta-get wine "region" 'note)))
         (concat "- region :: "
                 (vulpea-utils-link-make-string a))
       "")
     "\n"

     "- alcohol :: " (vulpea-note-meta-get wine "alcohol") "\n"
     "- sugar :: " (or (vulpea-note-meta-get wine "sugar") "N/A") "\n"
     "- price :: " (let ((prices (vulpea-note-meta-get-list wine "price")))
                     (if (= 1 (seq-length prices))
                         (car prices)
                       (completing-read "Price: " prices nil t)))
     "\n"
     "- importer :: Wine Bureau")))

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
    (cl-letf (((symbol-function 'dummy-prompt)
               (lambda (_prompt choices &optional display-fn)
                 (or (cl-find name choices :key display-fn :test #'string=)
                     (throw 'notfound nil)))))
      (dlet ((yas-prompt-functions '(dummy-prompt)))
        (catch 'notfound
          (yas-insert-snippet t))))))



(provide 'lib-bg-slides)
;;; lib-bg-slides.el ends here
