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

(require '+org-buffer-prop)
(require 'init-file)
(require 'lib-list)
(require 's)

(defvar +org-notes-directory nil)

;; compiler
(defvar time-stamp-start)
(defvar org-roam-last-window)
(defvar org-roam-buffer)
(defvar org-directory)
(defvar org-attach-id-dir)
(defvar org-link-bracket-re)
(defvar org-any-link-re)
(declare-function seq-contains-p "seq")
(declare-function deft "deft")
(declare-function deft-refresh "deft")
(declare-function org-read-date "org")
(declare-function org-back-to-heading "org")
(declare-function org-get-tags "org")
(declare-function org-set-tags "org")
(declare-function org-element-lineage "org-element")
(declare-function org-element-context "org-element")
(declare-function org-element-property "org-element")
(declare-function org-with-point-at "org-macs")
(declare-function org-roam-find-file "org-roam")
(declare-function org-roam--get-backlinks "org-roam")
(declare-function org-roam-insert "org-roam")
(declare-function org-roam-mode "org-roam")
(declare-function org-roam-tag-add "org-roam")
(declare-function org-roam-tag-delete "org-roam")
(declare-function org-roam-alias-add "org-roam")
(declare-function org-roam-alias-delete "org-roam")
(declare-function org-roam--extract-tags "org-roam")
(declare-function org-roam--extract-tags-prop "org-roam")
(declare-function org-roam--list-all-files "org-roam")
(declare-function org-roam--set-global-prop "org-roam")
(declare-function org-roam-format-link "org-roam")
(declare-function org-roam-db-query "org-roam-db")
(declare-function org-roam-db-build-cache "org-roam-db")
(declare-function org-roam-db--clear "org-roam-db")
(declare-function org-roam-db--insert-tags "org-roam-db")
(declare-function org-roam-db--update-tags "org-roam-db")
(declare-function org-roam "org-roam-buffer")
(declare-function org-roam-buffer--visibility "org-roam-buffer")
(declare-function org-roam--current-visibility "org-roam-buffer")

(autoload 'org-roam-dailies-find-today "org-roam-dailies")
(autoload 'org-roam-dailies-find-date "org-roam-dailies")
(autoload 'org-roam-dailies-find-next-note "org-roam-dailies")
(autoload 'org-roam-dailies-find-previous-note "org-roam-dailies")
(autoload 'org-roam-dailies-capture-today "org-roam-dailies")
(autoload 'org-roam-dailies-capture-date "org-roam-dailies")

(defun +org-notes-list ()
  "Open a list of notes."
  (interactive)
  (deft)
  (deft-refresh))

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

(defun +org-notes-ensure-filetag ()
  "Add missing file tags to the current note."
  (let ((tags (org-roam--extract-tags))
        (filetags (ignore-errors (+org-buffer-prop-get-list "FILETAGS")))
        (tag (+org-notes--title-as-tag)))
    (when (and (seq-contains-p tags "people")
               (not (seq-contains-p filetags tag)))
      (+org-buffer-prop-set
       "FILETAGS"
       (combine-and-quote-strings (seq-uniq (cons tag filetags))))
      (save-buffer))))

(defun +org-notes-ensure-tags ()
  "Add missing roam tags to the current note."
  (interactive)
  (let* ((file (buffer-file-name (buffer-base-buffer)))
         (all-tags (org-roam--extract-tags file))
         (tags (org-roam--extract-tags-prop file))
         (extra))
    (cond
     ((seq-contains-p all-tags "litnotes")
      (unless (seq-find (lambda (x) (string-prefix-p "Status:" x)) tags)
        (setq extra (cons "Status:New" extra)))
      (unless (seq-find (lambda (x) (string-prefix-p "Content:" x)) tags)
        (setq extra (cons
                     (concat "Content:"
                             (completing-read "Content: "
                                              '("Book" "Article" "Video" "Course")))
                     extra)))))
    (unless (null extra)
      (org-roam--set-global-prop
       "ROAM_TAGS"
       (combine-and-quote-strings (seq-uniq (append extra tags))))
      (org-roam-db--insert-tags 'update)
      (save-buffer))))

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
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (+org-notes-fix-links)
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

(defun +org-notes-get-file-by-id (id)
  "Get file of note with ID."
  (+seq-singleton
   (+seq-flatten
    (org-roam-db-query
     [:select file
      :from ids
      :where (= id $s1)]
     id))))

(defun +org-notes-get-title-by-id (id)
  "Get title of note with ID."
  (when-let* ((fls
               (org-roam-db-query
                [:select [file level]
                 :from ids
                 :where (= id $s1)]
                id))
              (fl (+seq-singleton fls))
              (file (car fl))
              (level (nth 1 fl)))
    (if (= 0 level)
        (org-roam-db--get-title file)
      (+org-with-file file
        (goto-char (cdr (org-id-find-id-in-file id file)))
        (+org-entry-get "ITEM")))))

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
               (desc (+org-notes-get-title-by-id id))
               (begin (org-element-property :begin link))
               (end (- (org-element-property :end link)
                       (org-element-property :post-blank link))))
          (delete-region begin end)
          (insert (org-roam-format-link id desc)))))))

(defun +org-notes-meta (id)
  "Get metadata for note with ID.

Return plist (:file :buffer :pl)

Metadata is defined by the first description list in the note,
e.g. list like:

- key1 :: value1
- key2 :: value21
- key2 :: value22
- key3 :: value3

In most cases, it's better to use either `+org-notes-meta-get' to
retrieve a single value for a given key or
`+org-notes-meta-get-list' to retrieve all values for a given
key."
  (when-let ((file (+org-notes-get-file-by-id id)))
    (+org-with-file file
      (let* ((buf (org-element-parse-buffer))
             (pls (org-element-map buf 'plain-list #'identity))
             (pl (seq-find
                  (lambda (pl)
                    (equal 'descriptive
                           (org-element-property :type pl)))
                  pls)))
        (list :file file
              :buffer buf
              :pl pl)))))

(defun +org-notes-meta--get (id prop)
  "Get all values of PROP for note with ID.

Return plist (:file :buffer :pl :items)"
  (let* ((meta (+org-notes-meta id))
         (pl (plist-get meta :pl))
         (items-all (org-element-map pl 'item #'identity))
         (items
          (seq-filter
           (lambda (item)
             (string-equal
              prop
              (org-element-interpret-data
               (org-element-contents (org-element-property :tag item)))))
           items-all)))
    (plist-put meta :items items)))

(defun +org-notes-meta-get-list (id prop &optional type)
  "Get all values of PROP for note with ID.

Each element value depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- id - id of the linked note."
  (setq type (or type 'string))
  (let* ((meta (+org-notes-meta--get id prop))
         (items (plist-get meta :items)))
    (seq-map
     (lambda (item)
       (let ((val (car (org-element-contents item))))
         (pcase type
           (`raw val)
           (`string (s-trim-right
                     (substring-no-properties
                      (org-element-interpret-data (org-element-contents val)))))
           (`id (let ((el (car (org-element-contents val))))
                  (when (equal 'link
                               (org-element-type el))
                    (org-element-property :path el)))))))
     items)))

(defun +org-notes-meta-get (id prop &optional type)
  "Get value of PROP for note with ID.

Result depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- id - id of the linked note.

If the note contains multiple values for a given PROP, the first
one is returned. In case all values are required, use
`+org-notes-meta-get-list'."
  (car (+org-notes-meta-get-list id prop type)))

(defun +org-notes-meta-set (id prop value)
  "Set VALUE of PROP for note with ID."
  (let* ((meta (+org-notes-meta--get id prop))
         (file (plist-get meta :file))
         (buffer (plist-get meta :buffer))
         (pl (plist-get meta :pl))
         (items (plist-get meta :items))
         (img (org-element-copy (car items))))
    (+org-with-file file
      (cond
       (pl
        ;; TODO: inline
        (+org-notes-meta-remove id prop)
        (cond
         (img
          (goto-char (org-element-property :begin img))
          (insert (org-element-interpret-data
                   (org-element-set-contents (org-element-copy img) value)))
          (when (equal (length items)
                       (length (org-element-contents pl)))
            (insert "\n")))
         (t
          (let* ((items-all (org-element-map pl 'item #'identity))
                 ;; we copy any item from the list so we don't need to deal with
                 ;; :bullet and other properties
                 (img (org-element-copy (car items-all))))
            (goto-char (org-element-property :begin pl))
            (insert (org-element-interpret-data
                     (org-element-set-contents
                      (org-element-put-property (org-element-copy img) :tag prop)
                      value)))))))
       (t
        ;; insert either after the last keyword in the buffer, or after the
        ;; property drawer if it is present on the first line or on the fist
        ;; line
        (let* ((element (or (car (last (org-element-map buffer 'keyword #'identity)))
                            (car (org-element-map buffer 'property-drawer #'identity))))
               (point (if element
                          (org-element-property :end element)
                        1)))
          (goto-char point)
          (insert "- " prop " :: " value "\n\n")))))))

(defun +org-notes-meta-remove (id prop)
  "Delete values of PROP for note with ID."
  (let* ((meta (+org-notes-meta--get id prop))
         (items (plist-get meta :items))
         (pl (plist-get meta :pl))
         (file (plist-get meta :file)))
    (when (car items)
      (+org-with-file file
        (if (equal (length items)
                   (length (org-element-contents pl)))
            (delete-region (org-element-property :begin pl)
                           (org-element-property :end pl))
          (seq-do
           (lambda (item)
             (when-let* ((begin (org-element-property :begin item))
                         (end (org-element-property :end item)))
               (delete-region begin end)))
           (seq-reverse items)))))))

(provide '+org-notes)
;;; +org-notes.el ends here
