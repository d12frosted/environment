;;; lib-vulpea.el --- Vulpea utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
(require 'svg-lib)
(require 'svg-tag-mode)



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
      (let ((todo-type (org-element-property :todo-type h)))
        (or
         ;; any headline with some todo keyword
         (eq 'todo todo-type)
         ;; any headline with REFILE tag
         (seq-contains-p (org-element-property :tags h) "REFILE")
         ;; any non-todo headline with an active timestamp
         (and
          (not (eq 'done todo-type))
          (save-excursion
            (goto-char (org-element-property :contents-begin h))
            (let ((end (save-excursion
                         ;; we must look for active timestamps only
                         ;; before then next heading, even if it's
                         ;; child, but org-element-property
                         ;; :contents-end includes all children
                         (or
                          (re-search-forward org-element-headline-re
                                             (org-element-property :contents-end h)
                                             ':noerror)
                          (org-element-property :contents-end h)))))
              (re-search-forward org-ts-regexp end 'noerror)))))))
    nil 'first-match))



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
      (org-fold-hide-drawer-toggle 'off))
    (setq-local tab-width 1)
    (vulpea-ensure-filetag)))

;;;###autoload
(defun vulpea-pre-save-hook ()
  "Do all the dirty stuff when file is being saved."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (vulpea-ensure-filetag)))



(defconst vulpea-svg-tag-face 'org-link)
(defconst vulpea-svg-tag-style
  (svg-lib-style-compute-default vulpea-svg-tag-face))

;;;###autoload
(defun vulpea-setup-svg-tags ()
  "Do something useful."
  (setq-local
   svg-tag-tags
   (list
    (cons
     ;; This trick allows to leave description of the link and yet to
     ;; append an icon before description; works good when
     ;; `org-toggle-link-display' is enabled.
     ;;
     ;; Ideally we should use `svg-lib-concat', but it doesn't
     ;; calculate the high properly.
     ;;
     ;; (svg-lib--image (svg-lib-concat ICON TAG))
     (concat "\\(" (s-left 2 org-link-bracket-re) "\\)"
             (s-right (- (length org-link-bracket-re) 2) org-link-bracket-re))
     (list
      (lambda (_)
        (when-let* ((link (match-string 2))
                    (id (when (string-prefix-p "id:" link)
                          (string-remove-prefix "id:" link)))
                    (note (vulpea-db-get-by-id id))
                    (tags (vulpea-note-tags note)))
          (cond
           ((seq-contains-p tags litnotes-tag)
            (litnotes-content-icon
             (litnotes-entry-content
              (litnotes-entry note))
             :face vulpea-svg-tag-face))

           (t (when-let ((data
                          (cond
                           ((seq-contains-p tags "people")
                            '("bootstrap" "person"))
                           ((seq-contains-p tags "grape")
                            '("custom" "grapes"))
                           ((seq-contains-p tags "cellar")
                            '("fa-solid" "wine-glass"))
                           ((seq-contains-p tags "appellation")
                            '("fa-solid" "location-arrow"))
                           ((seq-contains-p tags "region")
                            '("fa-solid" "location-arrow"))
                           ((seq-contains-p tags "places")
                            '("fa-solid" "location-arrow"))
                           ((seq-contains-p tags "producer")
                            '("bootstrap" "person"))

                           ((seq-contains-p tags "aroma")))))
                (svg-lib-icon (nth 1 data) vulpea-svg-tag-style
                              :collection (nth 0 data)
                              :stroke 0))))))))))
  (svg-tag-mode))



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



(cl-defun vulpea-db-process-notes (&key
                                   filter-fn
                                   process-fn
                                   quiet)
  "Process `vulpea-note's.

Mostly used for simple migrations.

This function has the following features and properties.

- It tries to avoid performance degradation even when 10k+ notes
  are being processed.

- Each (id . file) pair is processed only once, meaning that
  PROCESS-FN is not called multiple times on the same node.
  Despite this, keep your PROCESS-FN idempotent.

- Point is placed at the beginning of note. Meaning that for
  file-level notes the point is at the beginning of buffer; and
  for heading-level notes the point is at the beginning of
  heading.

- Progress is printed on the go unless QUIET is non-nil.

Notes are selected by using FILTER-FN which takes `vulpea-note'
as its only argument and returns non-nil if the note needs to be
selected.

PROCESS-FN is called with `vulpea-note' as it's argument. Result
is ignored. Any buffer modification is saved."
  (let* ((notes (seq-filter
                 (lambda (n) (null (vulpea-note-primary-title n)))
                 (vulpea-db-query filter-fn)))
         (count (seq-length notes)))
    (unless quiet
      (pcase count
        (`0 (message "No notes to process"))
        (`1 (message "Processing 1 note"))
        (_ (message "Processing %d note" count))))
    (seq-map-indexed
     (lambda (n i)
       (let ((level (vulpea-note-level n))
             (file-name (file-name-nondirectory (vulpea-note-path n)) ))
         (unless quiet
           (message
            (s-truncate
             80
             (format
              "[%s/%d] Processing %s"
              (string-from-number (+ i 1) :padding-num count)
              count
              (if (= 0 level)
                  file-name
                (concat file-name "#" (vulpea-note-title n))))))))
       (cl-letf (((symbol-function 'message) (lambda (&rest _))))
         (vulpea-visit n))
       (when process-fn
         (funcall process-fn n))
       (save-buffer)
       (kill-buffer))
     notes)))

;;;###autoload
(defun vulpea-db-build ()
  "Update notes database."
  (when (file-directory-p vulpea-directory)
    (org-roam-db-sync)
    (vino-db-sync)
    (org-roam-update-org-id-locations)
    (org-persist-gc)
    (org-persist-write-all)))



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
(defun vulpea-visit (note-or-id &optional other-window)
  "Visit NOTE-OR-ID.

If OTHER-WINDOW, visit the NOTE in another window."
  (let ((id (if (vulpea-note-p note-or-id)
                (vulpea-note-id note-or-id)
              note-or-id)))
    (org-roam-node-visit
     (org-roam-node-from-id id)
     (or current-prefix-arg
         other-window))))

;;;###autoload
(defun vulpea-buttonize (note &optional title-fn)
  "Create a link to `vulpea' NOTE.

Title is calculated based on TITLE-FN (takes note as a single
parameter), defaulting to `vulpea-note-title'."
  (buttonize (funcall (or title-fn #'vulpea-note-title)
                      note)
             #'vulpea-visit
             (vulpea-note-id note)))



;;;###autoload
(defun vulpea-review-random ()
  "Visit random `vulpea-note' for review."
  (interactive)
  (if-let ((notes (seq-sort-by
                   (lambda (note)
                     (or (vulpea-note-meta-get note "last review")
                         "[1972-01-01]"))
                   #'org-time<
                   (vulpea-db-query
                    (lambda (note)
                      ;; include only file-level notes
                      (= 0 (vulpea-note-level note)))))))
      (vulpea-visit (nth (random (min 10 (seq-length notes)))
                         notes))
    (message "No notes no review.")))

;;;###autoload
(defun vulpea-review-complete ()
  "Mark currently visiting `vulpea-note' as reviewed."
  (interactive)
  (vulpea-buffer-meta-set
   "last review"
   (format-time-string
    (org-time-stamp-format 'long 'inactive))
   'append))



(provide 'lib-vulpea)
;;; lib-vulpea.el ends here
