;;; +org-notes.el --- note taking helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 03 Apr 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require '+org)
(require '+org-prop)
(require '+org-buffer-prop)
(require 'init-file)
(require 'lib-list)
(require '+string)
(require 's)

(defvar +org-notes-directory nil)

(defun +org-notes-select (prompt &optional initial-prompt completions filter-fn)
  "Select a note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.

FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`+org-notes--get-title-path-completions' for details."
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (or completions
                          (+org-notes--get-title-path-completions)))
         (completions (if filter-fn
                          (seq-filter filter-fn completions)
                        completions))
         (title-with-tags (org-roam-completion--completing-read (concat prompt ": ") completions
                                                                :initial-input initial-prompt))
         (res (cdr (assoc title-with-tags completions)))
         (id (vulpea-db-get-id-by-file (plist-get res :path))))
    (if id
        (plist-put res :id id)
      (plist-put res :title title-with-tags))))

(defun +org-notes--get-title-path-completions ()
  "Return an alist for completion.

The car is the displayed title for completion, and the cdr
contains all the funny stuff."
  (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta] :from titles
                                   :left :join tags
                                   :on (= titles:file tags:file)
                                   :left :join files
                                   :on (= titles:file files:file)]))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--prepend-tag-string title tags))
              (v (list :path file-path :title title :tags tags)))
          (push (cons k v) completions))))))

(defun +org-notes-find ()
  "Find a note."
  (interactive)
  (org-roam-find-file))

(defun +org-notes-find-backlink ()
  "Find a note linked to current note."
  (interactive)
  (when-let* ((buffer (current-buffer))
              (file (buffer-file-name buffer))
              (backlinks (seq-uniq (seq-map #'car (org-roam--get-backlinks file)))))
    (org-roam-find-file
     nil
     nil
     (lambda (completions)
       (seq-filter
        (lambda (x) (seq-contains-p backlinks (plist-get (cdr x) :path)))
        completions)))))

(defun +org-notes-insert ()
  "Insert a link to the note."
  (interactive)
  (when-let*
      ((res (org-roam-insert))
       (path (plist-get res :path))
       (title (plist-get res :title))
       (roam-tags (+seq-flatten
                   (+seq-flatten
                    (org-roam-db-query [:select tags
                                        :from tags
                                        :where (= file $s1)]
                                       path)))))
    (when (seq-contains-p roam-tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-set-tags
           (seq-uniq
            (cons
             (+org-notes--title-to-tag title)
             (org-get-tags nil t)))))))))

(defun +org-notes-new-journal-entry ()
  "Create new journal entry.

By default it uses current date to find a daily. With
\\[universal-argument] user may select the date."
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))     ; select date
    (org-roam-dailies-capture-date))
   (t
    (org-roam-dailies-capture-today))))

(defun +org-notes-dailies-today ()
  "Find a daily note for today."
  (interactive)
  (org-roam-dailies-find-today))

(defun +org-notes-dailies-date ()
  "Find a daily note for date specified using calendar."
  (interactive)
  (org-roam-dailies-find-date))

(defun +org-notes-dailies-prev ()
  "Find a daily note that comes before current."
  (interactive)
  (org-roam-dailies-find-previous-note))

(defun +org-notes-dailies-next ()
  "Find a daily note that comes after current."
  (interactive)
  (org-roam-dailies-find-next-note))

(defun +org-notes-setup-buffer (&optional _)
  "Setup current buffer for notes viewing and editing.

If the current buffer is not a note, does nothing."
  (when (and (not (active-minibuffer-window))
             (+org-notes-buffer-p))
    (unless (bound-and-true-p org-roam-mode)
      (org-roam-mode 1))
    (setq-local time-stamp-start "#\\+TIME-STAMP:[ 	]+\\\\?[\"<]+")
    (setq-local org-attach-id-dir (expand-file-name ".data" org-directory))
    (setq-local org-preview-latex-image-directory (expand-file-name ".ltximg/" org-directory))
    (+org-notes-ensure-filetag)
    (+org-notes-ensure-tags)))

(defun +org-notes-pre-save-hook ()
  "Do all the dirty stuff when file is being saved."
  (when (and (not (active-minibuffer-window))
             (+org-notes-buffer-p))
    (+org-notes-ensure-filetag)
    (+org-notes-ensure-tags)))

(defun +org-notes-ensure-filetag ()
  "Add missing file tags to the current note."
  (let ((tags (org-roam--extract-tags))
        (filetags (ignore-errors (+org-buffer-prop-get-list "FILETAGS")))
        (tag (+org-notes--title-as-tag)))
    (when (and (seq-contains-p tags "people")
               (not (seq-contains-p filetags tag)))
      (+org-buffer-prop-set
       "FILETAGS"
       (combine-and-quote-strings (seq-uniq (cons tag filetags)))))))

(defun +org-notes-ensure-tags ()
  "Add missing roam tags to the current note."
  (interactive)
  (let* ((file (buffer-file-name (buffer-base-buffer)))
         (all-tags (org-roam--extract-tags file))
         (prop-tags (org-roam--extract-tags-prop file))
         (tags prop-tags))

    ;; process litnotes
    (when (seq-contains-p all-tags "litnotes")
      (unless (seq-find (lambda (x) (string-prefix-p "Status:" x)) tags)
        (setq tags (cons "Status:New" tags)))
      (unless (seq-find (lambda (x) (string-prefix-p "Content:" x)) tags)
        (setq tags (cons
                    (concat "Content:"
                            (completing-read "Content: "
                                             '("Book" "Article" "Video" "Course")))
                    tags))))

    ;; process projects
    (if-let ((projectp
              (seq-find (lambda (type)
                          (eq type 'todo))
                        (org-element-map
                            (org-element-parse-buffer 'headline)
                            'headline
                          (lambda (h)
                            (and (org-element-property :todo-keyword h)
                                 (org-element-property :todo-type h)))))))
        (setq tags (cons "Project" tags))
      (setq tags (remove "Project" tags)))
    (unless (eq prop-tags tags)
      (org-roam--set-global-prop
       "ROAM_TAGS"
       (combine-and-quote-strings (seq-uniq tags))))))

(defun +org-notes-set-status ()
  "Change status tag of current note."
  (interactive)
  (when-let* ((file (buffer-file-name (buffer-base-buffer)))
              (tags (org-roam--extract-tags-prop file))
              (status-raw (completing-read
                           "Status: "
                           '("New" "Ongoing" "Done" "Dropped")))
              (status (concat "Status:" status-raw))
              (new-tags (cons status
                              (seq-remove (lambda (x) (string-prefix-p "Status:" x))
                                          tags))))
    (org-roam--set-global-prop "ROAM_TAGS" (combine-and-quote-strings new-tags))
    (org-roam-db--insert-tags 'update)
    (save-buffer)))

(defun +org-notes-rebuild ()
  "Rebuild notes database."
  (interactive)
  (dolist (file (org-roam--list-all-files))
    (message "processing %s" file)
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (+org-notes-fix-links)
      (+org-notes-ensure-tags)
      (+org-notes-ensure-filetag)
      (save-buffer)))
  (org-roam-db--clear)
  (org-roam-db-build-cache))

(defun +org-notes-tags-add ()
  "Add a tag to current note."
  (interactive)
  (when (org-roam-tag-add)
    (+org-notes-ensure-filetag)))

(defun +org-notes-tags-delete ()
  "Delete a tag from current note."
  (interactive)
  (org-roam-tag-delete))

(defun +org-notes-alias-add ()
  "Add an alias to current note."
  (interactive)
  (org-roam-alias-add))

(defun +org-notes-alias-delete ()
  "Delete an alias from current note."
  (interactive)
  (org-roam-alias-delete))

(defun +org-notes-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory +org-notes-directory))
        (file-name-directory buffer-file-name))))

(defun +org-notes--title-as-tag ()
  "Return title of the current note as tag."
  (+org-notes--title-to-tag (+org-buffer-prop-get "TITLE")))

(defun +org-notes--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))

(defun +org-notes-subdir ()
  "Select notes subdirectory."
  (interactive)
  (let ((dirs (cons "."
                    (seq-map
                     (lambda (p)
                       (string-remove-prefix +org-notes-directory p))
                     (+file-subdirs +org-notes-directory nil t)))))
    (completing-read "Subdir: " dirs nil t)))

(defun +org-notes-fix-links ()
  "Fixes the links to Org Roam notes in the current buffer."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (let ((desc (match-string 2)))
        (when-let ((link (save-match-data (org-element-lineage (org-element-context) '(link) t))))
          (when (string-equal "file" (org-element-property :type link))
            (let ((path (expand-file-name (org-element-property :path link))))
              (replace-match "")
              (insert (org-roam-format-link path desc))))))))
  (org-with-point-at 1
    (while (re-search-forward "id:\\.\\." nil t)
      (when-let ((link (org-element-lineage (org-element-context) '(link) t)))
        (let* ((id (+string-chop-prefix-regexp ".+/" (org-element-property :path link)))
               (note (vulpea-db-get-by-id id))
               (desc (plist-get note :title))
               (begin (org-element-property :begin link))
               (end (- (org-element-property :end link)
                       (org-element-property :post-blank link))))
          (delete-region begin end)
          (insert (org-roam-format-link id desc)))))))

(provide '+org-notes)
;;; +org-notes.el ends here
