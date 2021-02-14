;;; lib-vulpea.el --- Vulpea utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 11 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
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
;; Various utilities extending `vulpea' and `org-roam'.
;;
;;; Code:

(require 'init-elpa)
(require 'init-vulpea)

(require 'lib-directory)

(require 'org-roam)
(require 'org-roam-dailies)



(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (eq major-mode 'org-mode)
       (string-prefix-p
        (expand-file-name (file-name-as-directory vulpea-directory))
        (file-name-directory buffer-file-name))))



;;;###autoload
(defun vulpea-find ()
  "Find a note."
  (interactive)
  (org-roam-find-file))

;;;###autoload
(defun vulpea-find-backlink ()
  "Find a note linked to current note."
  (interactive)
  (when-let* ((buffer (current-buffer))
              (file (buffer-file-name buffer))
              (backlinks (seq-uniq
                          (seq-map
                           #'car
                           (org-roam--get-backlinks file)))))
    (org-roam-find-file
     nil
     nil
     (lambda (completions)
       (seq-filter
        (lambda (x) (seq-contains-p backlinks
                                    (plist-get (cdr x) :path)))
        completions)))))



;;;###autoload
(defun vulpea-insert ()
  "Insert a link to the note."
  (interactive)
  (when-let*
      ((res (org-roam-insert))
       (path (plist-get res :path))
       (title (plist-get res :title))
       (roam-tags (org-roam-with-file path nil
                    (org-roam--extract-tags path))))
    (when (seq-contains-p roam-tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-set-tags
           (seq-uniq
            (cons
             (vulpea--title-to-tag title)
             (org-get-tags nil t)))))))))



;;;###autoload
(defun vulpea-tags-add ()
  "Add a tag to current note."
  (interactive)
  (when (org-roam-tag-add)
    (vulpea-ensure-filetag)))

;;;###autoload
(defun vulpea-tags-delete ()
  "Delete a tag from current note."
  (interactive)
  (org-roam-tag-delete))

;;;###autoload
(defun vulpea-ensure-filetag ()
  "Add missing FILETAGS to the current note."
  (let ((tags (org-roam--extract-tags))
        (filetags (ignore-errors
                    (vulpea-buffer-prop-get-list "FILETAGS")))
        (tag (vulpea--title-as-tag)))
    (when (and (seq-contains-p tags "people")
               (not (seq-contains-p filetags tag)))
      (vulpea-buffer-prop-set
       "FILETAGS"
       (combine-and-quote-strings (seq-uniq (cons tag filetags)))))))

;;;###autoload
(defun vulpea-ensure-roam-tags ()
  "Add missing ROAM tags to the current note.")



;;;###autoload
(defun vulpea-alias-add ()
  "Add an alias to current note."
  (interactive)
  (org-roam-alias-add))

;;;###autoload
(defun vulpea-alias-delete ()
  "Delete an alias from current note."
  (interactive)
  (org-roam-alias-delete))



;;;###autoload
(defun vulpea-pre-save-hook ()
  "Do all the dirty stuff when file is being saved."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (vulpea-ensure-filetag)
    (vulpea-ensure-roam-tags)))



;;;###autoload
(defun vulpea-dailies-today ()
  "Find a daily note for today."
  (interactive)
  (org-roam-dailies-find-today))

;;;###autoload
(defun vulpea-dailies-date ()
  "Find a daily note for date specified using calendar."
  (interactive)
  (org-roam-dailies-find-date))

;;;###autoload
(defun vulpea-dailies-prev ()
  "Find a daily note that comes before current."
  (interactive)
  (org-roam-dailies-find-previous-note))

;;;###autoload
(defun vulpea-dailies-next ()
  "Find a daily note that comes after current."
  (interactive)
  (org-roam-dailies-find-next-note))



;;;###autoload
(defun vulpea-subdir-select ()
  "Select notes subdirectory."
  (interactive)
  (let ((dirs (cons
               "."
               (seq-map
                (lambda (p)
                  (string-remove-prefix vulpea-directory p))
                (directory-subdirs vulpea-directory 'recursive)))))
    (completing-read "Subdir: " dirs nil t)))



(defun vulpea--title-as-tag ()
  "Return title of the current note as tag."
  (vulpea--title-to-tag (vulpea-buffer-prop-get "TITLE")))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))



(provide 'lib-vulpea)
;;; lib-vulpea.el ends here
