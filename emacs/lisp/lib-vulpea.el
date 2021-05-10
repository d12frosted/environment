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
  "Return a list of note files containing Project tag."
  (seq-map
   #'car
   (org-roam-db-query
    [:select file
     :from tags
     :where (like tags (quote "%\"Project\"%"))])))



;;;###autoload
(defun vulpea-find (&optional nodes require-match)
  "Find and open a note.

Unless list of NODES is given, all notes are subject of
selection. Each node is `org-roam-node'.

When REQUIRE-MATCH is nil user may select a non-existent note and
start the capture process."
  (interactive)
  (let ((node (org-roam-node-read
               nil
               (lambda (ns)
                 (if nodes
                     (seq-filter
                      (lambda (n)
                        (seq-contains-p
                         nodes (cdr n)
                         (lambda (a b)
                           (string-equal (org-roam-node-id a)
                                         (org-roam-node-id b)))))
                      ns)
                   ns)))))
    (if (org-roam-node-file node)
        (org-roam-node-visit node)
      (when (not require-match)
        (org-roam-capture-
         :node node
         :props '(:finalize find-file))))))

;;;###autoload
(defun vulpea-find-backlink ()
  "Find a note linked to current note."
  (interactive)
  (let* ((node (org-roam-node-at-point 'assert))
         (backlinks (seq-map
                     #'org-roam-backlink-source-node
                     (org-roam-backlinks-get node))))
    (vulpea-find backlinks 'require-match)))



;;;###autoload
(defun vulpea-insert ()
  "Insert a link to the note."
  (interactive)
  (when-let*
      ((node (org-roam-node-insert))
       (title (org-roam-node-title node))
       (tags (org-roam-node-tags node)))
    (when (seq-contains-p tags "people")
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
  (when (call-interactively #'org-roam-tag-add)
    (vulpea-ensure-filetag)))

;;;###autoload
(defun vulpea-tags-delete ()
  "Delete a tag from current note."
  (interactive)
  (call-interactively #'org-roam-tag-remove))

;;;###autoload
(defun vulpea-ensure-filetag ()
  "Add missing FILETAGS to the current note."
  (let ((tags (vulpea-buffer-tags-get))
        (tag (vulpea--title-as-tag)))
    (when (and (seq-contains-p tags "people")
               (not (seq-contains-p tags tag)))
      (vulpea-buffer-tags-add tag))))

;;;###autoload
(defun vulpea-ensure-roam-tags ()
  "Add missing ROAM tags to the current note."
  (save-excursion
    (goto-char (point-min))
    (let* ((tags (vulpea-buffer-tags-get))
           (original-tags tags))

      ;; process litnotes
      (when (seq-contains-p tags "litnotes")
        (unless (org-entry-get (point) "ROAM_REFS")
          (org-roam-ref-add (read-string "URL: ")))
        (unless (seq-find (lambda (x)
                            (string-prefix-p "status/" x))
                          tags)
          (setq tags (cons "status/new" tags)))
        (unless (seq-find (lambda (x)
                            (string-prefix-p "content/" x))
                          tags)
          (setq tags (cons
                      (concat
                       "Content:"
                       (completing-read
                        "content/"
                        '("book" "article" "video" "course")))
                      tags))))

      ;; process projects
      (if (vulpea-project-p)
          (setq tags (cons "project" tags))
        (setq tags (remove "project" tags)))
      (unless (eq original-tags tags)
        (apply #'vulpea-buffer-tags-set (seq-uniq tags))))))



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
  ;; TODO: once better aliases are merged stop doing things
  ;; manually.
  (if-let* ((node (org-roam-node-at-point 'assert))
            (aliases (org-roam-node-aliases node)))
      (let* ((alias (completing-read
                     "Alias: " aliases nil 'require-match))
             (backlinks (seq-map
                         #'org-roam-backlink-source-node
                         (org-roam-backlinks-get node)))
             (id-old (org-roam-node-id node))
             (aliases (delete alias aliases)))
        (if aliases
            (org-set-property "ROAM_ALIASES"
                              (combine-and-quote-strings aliases))
          (org-delete-property "ROAM_ALIASES"))
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
(defun vulpea-status-set ()
  "Change status tag of the current note."
  (interactive)
  (when-let*
      ((file (buffer-file-name (buffer-base-buffer)))
       (tags (vulpea-buffer-prop-get-list "filetags"))
       (status-raw (completing-read
                    "Status: "
                    '("new" "ongoing" "done" "dropped")))
       (status (concat "status/" status-raw))
       (new-tags (cons status
                       (seq-remove
                        (lambda (x)
                          (string-prefix-p "status/" x))
                        tags))))
    (vulpea-buffer-prop-set "filetags" new-tags)
    (org-roam-db-update-file file)
    (save-buffer)))



;;;###autoload
(defun vulpea-setup-buffer (&optional _)
  "Setup current buffer for notes viewing and editing."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (vulpea-ensure-filetag)
    (vulpea-ensure-roam-tags)))

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
(defun vulpea-db-build ()
  "Update notes database."
  (when (file-directory-p vulpea-directory)
    (org-roam-db-sync)
    (vulpea-migrate-db)))



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



(defun vulpea-migrate-buffer ()
  "Migrate current buffer note to `org-roam' v2."
  ;; Create file level ID if it doesn't exist yet
  (org-with-point-at 1
    (org-id-get-create))

  ;; update title (just to make sure it's lowercase)
  (vulpea-buffer-title-set (vulpea-buffer-prop-get "title"))

  ;; move roam_key into properties drawer roam_ref
  (when-let* ((ref (vulpea-buffer-prop-get "roam_key")))
    (org-set-property "ROAM_REFS" ref)
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_key:" (point-max) t)
          (beginning-of-line)
          (kill-line 1)))))

  ;; move roam_alias into properties drawer roam_aliases
  (when-let* ((aliases (vulpea-buffer-prop-get-list "roam_alias")))
    (org-set-property "ROAM_ALIASES"
                      (combine-and-quote-strings aliases))
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_alias:" (point-max) t)
          (beginning-of-line)
          (kill-line 1)))))

  ;; move roam_tags into filetags
  (let* ((roam-tags (vulpea-buffer-prop-get-list "roam_tags"))
         (file-tags (seq-filter
                     (lambda (x) (not (string-empty-p x)))
                     (split-string
                      (string-remove-prefix
                       org-roam-directory
                       (file-name-directory (buffer-file-name)))
                      "/")))
         (tags (seq-map
                (lambda (tag)
                  (setq tag (string-replace ":" "/" tag))
                  (setq tag (string-replace " " "_" tag))
                  (if (or
                       (string-prefix-p "status" tag 'ignore-case)
                       (string-prefix-p "content" tag 'ignore-case)
                       (string-equal "Project" tag))
                      (setq tag (downcase tag)))
                  tag)
                (seq-uniq (append roam-tags file-tags)))))
    (when tags
      (apply #'vulpea-buffer-tags-set tags)
      (let ((case-fold-search t))
        (org-with-point-at 1
          (while (re-search-forward "^#\\+roam_tags:" (point-max) t)
            (beginning-of-line)
            (kill-line 1))))))

  (save-buffer))

(defun vulpea-migrate-db ()
  "Migrate all notes."
  (dolist (f (org-roam--list-all-files))
    (with-current-buffer (find-file f)
      (message "migrating %s" f)
      (vulpea-migrate-buffer)))

  ;; Step 2: Build cache
  (org-roam-db-sync 'force))



(provide 'lib-vulpea)
;;; lib-vulpea.el ends here
