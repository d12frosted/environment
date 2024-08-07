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

(require 'config-vulpea)
(require 'lib-directory)
(require 'lib-svg)

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
         ;; any headline with REFILE tag (no inheritance)
         (seq-contains-p (org-element-property :tags h) "REFILE")
         ;; any non-todo headline with an active timestamp
         (and
          (not (eq 'done todo-type))
          (org-element-property :contents-begin h)
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
(defun vulpea-find-candidates (&optional filter)
  "Return list of candidates for `vulpea-find'.

FILTER is a `vulpea-note' predicate."
  (let ((notes (vulpea-db-query-by-tags-none '("cellar" "rating" "appellation" "grape" "region"))))
    (if filter
        (-filter filter notes)
      notes)))

;;;###autoload
(defun vulpea-insert-candidates (&optional filter)
  "Return list of candidates for `vulpea-find'.

FILTER is a `vulpea-note' predicate."
  (let ((notes (vulpea-db-query-by-tags-none '("cellar" "rating" "appellation" "grape" "region"))))
    (if filter
        (-filter filter notes)
      notes)))



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
    (setq-local tab-width 8)
    (vulpea-ensure-filetag)))

;;;###autoload
(defun vulpea-pre-save-hook ()
  "Do all the dirty stuff when file is being saved."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (vulpea-ensure-filetag)))



(defface vulpea-svg-tag-face `((t (:foreground ,(face-foreground 'org-link nil t)
                                   :inherit default)))
  "Faces used for svg tags in vulpea buffers."
  :group 'faces)

;;;###autoload
(defun vulpea-setup-svg-tags ()
  "Do something useful."
  (setq-local
   svg-tag-tags
   (list
    (cons
     (concat "\\(" org-link-bracket-re "\\)")
     (list
      (lambda (_)
        (when-let ((link (match-string 2)))
          (cond
           ((string-prefix-p "id:" link)
            (when-let* ((id (string-remove-prefix "id:" link))
                        (note (vulpea-db-get-by-id id)))
              (vulpea-note-to-svg note))))))))))
  (when env-graphic-p
    (svg-tag-mode)))

(defun vulpea-note-to-svg (note)
  "Return SVG representation of the NOTE."
  (let ((tags (vulpea-note-tags note))
        (scale 0.8)
        (padding 2))
    (when-let ((data
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
                 ((seq-contains-p tags "aroma")
                  '("bootstrap" "flower3")))))
      (svg-concat
       (svg-icon (nth 0 data) (nth 1 data)
                 :face 'vulpea-svg-tag-face
                 :scale scale)
       (svg-label (vulpea-note-title note)
                  :face 'vulpea-svg-tag-face
                  :padding padding)))))



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



(defmacro vulpea-utils-process-notes (notes &rest body)
  "Evaluate BODY for each element of NOTES.

Each element of NOTE in turn is bound to `it' and its index within NOTES
to `it-index' before evaluating BODY.

This function can be used for simple migrations as it provides some
useful features and properties:

- Visibility. Each step is logged, so the progress is visible.

- While result of BODY evaluation is discarded, any changes to the
  buffer are saved. For better performance, *all* Org mode buffers are
  *killed* after each step. Based on benchmarks, saving and killing
  buffers after each step is times faster than saving after all
  modifications.

- Each (id . file) pair is processed only once, meaning that BODY is not
  called multiple times on the same node. Despite this, keep your BODY
  idempotent.

- Point is placed at the beginning of note. Meaning that for
  file-level notes the point is at the beginning of buffer; and
  for heading-level notes the point is at the beginning of
  heading."
  (declare (debug (form body)) (indent 1))
  (let ((l (make-symbol "notes"))
        (i (make-symbol "i"))
        (count (make-symbol "count"))
        (countl (make-symbol "countl"))
        (level (make-symbol "level"))
        (file-name (make-symbol "file-name")))
    `(let* ((,l (-remove #'vulpea-note-primary-title ,notes))
            (,count (seq-length ,l))
            (,countl (number-to-string (length (number-to-string ,count))))
            (,i 0))
      (pcase ,count
       (`0 (message "No notes to process"))
       (`1 (message "Processing 1 note"))
       (_ (message "Processing %d notes" ,count)))
      (while ,l
       (let* ((it (pop ,l))
              (it-index ,i)
              (,level (vulpea-note-level it))
              (,file-name (file-name-nondirectory (vulpea-note-path it))))
        (message
         (s-truncate
          80
          (format
           (concat
            "[%" ,countl ".d/%d] Processing %s")
           (+ it-index 1)
           ,count
           (if (= 0 ,level)
               ,file-name
             (concat ,file-name "#" (vulpea-note-title it))))))
        (cl-letf (((symbol-function 'message) (lambda (&rest _))))
         (vulpea-visit it))
        (ignore it it-index)
        ,@body
        (save-some-buffers t)
        (kill-matching-buffers-no-ask ".*\\.org$"))
       (setq ,i (1+ ,i))))))

;;;###autoload
(defun vulpea-db-build ()
  "Update notes database."
  (when (file-directory-p vulpea-directory)
    (require 'vino)
    (require 'lib-brb)
    (org-roam-db-sync)
    (org-roam-update-org-id-locations)
    (org-persist-gc)
    (org-persist-write-all)

    ;; make sure that barberry/public is set on all notes that require it
    (vulpea-utils-process-notes (->> (vulpea-db-query-by-level 0)
                                     (--filter
                                      (and (not (vulpea-note-tagged-all-p it "barberry/public"))
                                           (or (vulpea-note-tagged-all-p it "wine" "grape")
                                               (vulpea-note-tagged-all-p it "wine" "region")
                                               (vulpea-note-tagged-all-p it "wine" "appellation")
                                               (vulpea-note-tagged-all-p it "places")))))
      (vulpea-buffer-tags-add "barberry/public"))

    ;; update sabotage links in wine entries
    (vulpea-utils-process-notes (->> (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                     (--filter (vulpea-note-meta-get it "externalId")))
      (when-let ((url (brb-sabotage-link (vulpea-note-meta-get it "externalId"))))
        (vulpea-buffer-meta-set "sabotage" url 'append)))

    ;; process missing files
    (--each (org-roam-list-files)
      (unless (vulpea-db-get-by-id (vulpea-db-get-id-by-file it))
        (message "Found a broken file at %s" it)
        (org-roam-db-clear-file it)
        (org-roam-db-update-file it)))

    (message " -> done building vulpea db")))



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
(cl-defun vulpea-meta-buttonize (note prop type callback
                                      &key default to-string)
  "Make a button for meta PROP of the NOTE.

Returns a button string with the value of PROP. When clicked,
user is prompted to input a new value that is set and saved in
the NOTE. Afterwards the CALLBACK is called with updated NOTE.

TYPE dictates how the value is parsed (see
`vulpea-note-meta-get').

When the value of PROP is nil, DEFAULT is used as a value.

TO-STRING controls how the value is formatted for button.
Defaults to `string-from'."
  (buttonize
   (funcall (or to-string #'string-from)
            (or (vulpea-note-meta-get note prop type) default))
   (lambda (&rest _)
     (let* ((prompt (s-capitalize prop))
            (value (pcase type
                     (`number (read-number (concat prompt ": ")))
                     (`note (vulpea-select prompt :require-match t))
                     (_ (read-string (concat prompt ": "))))))
       (vulpea-utils-with-note note
         (vulpea-buffer-meta-set prop value 'append)
         (save-buffer))
       (funcall callback (vulpea-db-get-by-id (vulpea-note-id note)))))))



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



;;;###autoload
(defun vulpea-db-setup-attachments ()
  "Setup attachments table in Vulpea DB."
  (vulpea-db-define-table
   'attachments 1
   '([(node-id :not-null)
      (file :not-null)
      (hash :not-null)]
     (:foreign-key
      [node-id]
      :references
      nodes [id]
      :on-delete
      :cascade))
   '((attachments-node-id [node-id])))
  (add-hook 'vulpea-db-insert-note-functions #'vulpea-db-insert-attachments))

(defun vulpea-db-insert-attachments (note)
  "Insert attachments of NOTE to database."
  (when-let ((dir (org-attach-dir)))
    (org-roam-db-query
     [:delete :from attachments
      :where (= node-id $s1)]
     (vulpea-note-id note))
    (--each (org-attach-file-list dir)
      (org-roam-db-query!
       (lambda (err)
         (lwarn 'org-roam :warning "%s for attachment '%s' in %s (%s) %s"
                (error-message-string err)
                it
                (vulpea-note-title note) (vulpea-note-id note) (vulpea-note-path note)))
       [:insert :into attachments
        :values $v1]
       (vector (vulpea-note-id note)
               it
               (s-trim
                (shell-command-to-string
                 (format "sha1sum '%s' | cut -d ' ' -f 1 -"
                         (expand-file-name it dir)))))))))



;;;###autoload
(defun vulpea-eval-all-code-blocks ()
  "Eval all code blocks in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+begin_src emacs-lisp" nil t)
      (let ((org-confirm-babel-evaluate nil))
        (funcall-interactively #'org-ctrl-c-ctrl-c))
      (forward-line 1))))

(provide 'lib-vulpea)
;;; lib-vulpea.el ends here
