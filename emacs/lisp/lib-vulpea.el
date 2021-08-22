;;; lib-vulpea.el --- Vulpea utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.2"))
;;
;; Created: 11 Feb 2021
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
;; Various utilities extending `vulpea' and `org-roam'.
;;
;;; Code:

(require 'init-elpa)

(require 'config-vulpea)
(require 'lib-directory)
(require 'lib-litnotes)

(require 'vulpea)
(require 'vino)
(require 'org-roam)
(require 'org-roam-db)
(require 'org-roam-dailies)



(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (eq major-mode 'org-mode)
       (string-suffix-p "org" buffer-file-name)
       (string-prefix-p
        (expand-file-name (file-name-as-directory vulpea-directory))
        (file-name-directory buffer-file-name))))



(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks. The only exception is headings tagged as REFILE."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (or (eq 'todo (org-element-property :todo-type h))
          (seq-contains-p (org-element-property :tags h)
                          "REFILE")))
    nil 'first-match))

;;;###autoload
(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))



;;;###autoload
(defun vulpea-insert-handle (note)
  "Hook to be called on NOTE after `vulpea-insert'."
  (when-let* ((title (vulpea-note-title note))
              (tags (vulpea-note-tags note)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-set-tags
             (seq-uniq
              (cons
               (vulpea--title-to-tag title)
               (org-get-tags nil t))))))))))



;;;###autoload
(defun vulpea-tags-add ()
  "Add a tag to current note."
  (interactive)
  (org-with-point-at 1
    (when (call-interactively #'org-roam-tag-add)
      (vulpea-ensure-filetag))))

;;;###autoload
(defun vulpea-tags-delete ()
  "Delete a tag from current note."
  (interactive)
  (call-interactively #'org-roam-tag-remove))

;;;###autoload
(defun vulpea-ensure-filetag ()
  "Add missing FILETAGS to the current note."
  (let* ((file (buffer-file-name))
         (path-tags
          (when file
            (seq-filter
             (lambda (x) (not (string-empty-p x)))
             (split-string
              (string-remove-prefix
               vulpea-directory
               (file-name-directory file))
              "/"))))
         (original-tags (vulpea-buffer-tags-get))
         (tags (append original-tags path-tags)))

    ;; process people
    (when (seq-contains-p tags "people")
      (let ((tag (vulpea--title-as-tag)))
        (unless (seq-contains-p tags tag)
          (setq tags (cons tag tags)))))

    ;; process litnotes
    (setq tags (litnotes-ensure-filetags tags))

    ;; process projects
    (if (vulpea-project-p)
        (setq tags (cons "project" tags))
      (setq tags (remove "project" tags)))

    (setq tags (seq-uniq tags))

    ;; update tags if changed
    (when (or (seq-difference tags original-tags)
              (seq-difference original-tags tags))
      (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))



;;;###autoload
(defun vulpea-alias-add ()
  "Add an alias to current note."
  (interactive)
  (call-interactively #'org-roam-alias-add))

;;;###autoload
(defun vulpea-alias-delete ()
  "Delete an alias from current note."
  (interactive)
  (call-interactively #'org-roam-alias-remove))

;;;###autoload
(defun vulpea-alias-extract ()
  "Extract an alias from current note as a separate note.

Make all the links to this alias point to newly created note."
  (interactive)
  (if-let* ((node (org-roam-node-at-point 'assert))
            (aliases (org-roam-node-aliases node)))
      (let* ((alias (completing-read
                     "Alias: " aliases nil 'require-match))
             (backlinks (seq-map
                         #'org-roam-backlink-source-node
                         (org-roam-backlinks-get node)))
             (id-old (org-roam-node-id node)))
        (org-roam-alias-remove alias)
        (org-roam-db-update-file (org-roam-node-file node))
        (let* ((note (vulpea-create
                      alias
                      "%<%Y%m%d%H%M%S>-${slug}.org"
                      :immediate-finish t
                      :unnarrowed t)))
          (seq-each
           (lambda (node)
             (vulpea-utils-with-file (org-roam-node-file node)
               (goto-char (point-min))
               (let ((link-old
                      (org-link-make-string
                       (concat "id:" id-old)
                       alias))
                     (link-new
                      (vulpea-utils-link-make-string note)))
                 (while (search-forward link-old nil 'noerror)
                   (replace-match link-new))))
             (org-roam-db-update-file (org-roam-node-file node)))
           backlinks)))
    (user-error "No aliases to extract")))



;;;###autoload
(defun vulpea-setup-buffer (&optional _)
  "Setup current buffer for notes viewing and editing."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (org-with-point-at 1
      (org-hide-drawer-toggle 'off))
    (setq-local tab-width 1)
    (vulpea-ensure-filetag)))

;;;###autoload
(defun vulpea-pre-save-hook ()
  "Do all the dirty stuff when file is being saved."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (vulpea-ensure-filetag)))



;;;###autoload
(defun vulpea-dailies-today ()
  "Find a daily note for today."
  (interactive)
  (org-roam-dailies-goto-today))

;;;###autoload
(defun vulpea-dailies-date ()
  "Find a daily note for date specified using calendar."
  (interactive)
  (org-roam-dailies-goto-date))

;;;###autoload
(defun vulpea-dailies-prev ()
  "Find a daily note that comes before current."
  (interactive)
  (org-roam-dailies-goto-previous-note))

;;;###autoload
(defun vulpea-dailies-next ()
  "Find a daily note that comes after current."
  (interactive)
  (org-roam-dailies-goto-next-note))



;;;###autoload
(defun vulpea-db-build ()
  "Update notes database."
  (when (file-directory-p vulpea-directory)
    (org-roam-db-sync)
    (vino-db-sync)))



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
  (vulpea--title-to-tag (vulpea-buffer-prop-get "title")))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))



;;;###autoload
;; org-check-agenda-file
(defun vulpea-check-agenda-file (&rest _)
  "A noop advice for `org-check-agenda-file'.

Since this function is called from multiple places, it is very
irritating to answer this question every time new note is created.

Also, it doesn't matter if the file in question is present in the
list of `org-agenda-files' or not, since it is built dynamically
via `vulpea-agenda-files-update'.")



;;;###autoload
(defun vulpea-activate-link (start end path _brackets)
  "Activate a link between START and END for PATH."
  (when (string-match-p string-uuid-regexp path)
    (when-let ((note (vulpea-db-get-by-id path)))
      (when (seq-contains-p (vulpea-note-tags note) litnotes-tag)
        (let* ((visible-start (or (match-beginning 3)
                                  (match-beginning 2)))
	             (visible-end (or (match-end 3) (match-end 2)))
               (entry (litnotes-entry note))
               (descr (concat (litnotes-content-display
                               (litnotes-entry-content entry)
                               :height 0.8 :v-adjust 0.04)
                              (buffer-substring
                               visible-start visible-end)))
               (descr (s-truncate (- end start) descr))
               (hidden `(invisible
			                   ,(or (org-link-get-parameter "id" :display)
				                      'org-link))))
          (remove-text-properties start visible-start hidden)
          (remove-text-properties visible-end end hidden)
          (add-text-properties (+ start (length descr)) end hidden)
          (put-text-property start end 'display descr))))))



(provide 'lib-vulpea)
;;; lib-vulpea.el ends here
