;;; lib-publicatorg.el --- Make org notes public -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 17 Jun 2022
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
;; TODO:
;;
;; - cleanup of affected files
;;
;;; Code:

(require 'vulpea)
(require 'lib-fun)
(require 'lib-vulpea)



(defvar porg--projects nil)



(cl-defstruct porg-project
  name
  root
  cache-file
  input
  describe
  rules)

(cl-defmethod porg-project-hash (project &optional ignore-rules)
  "Calculate hash of the PROJECT.

When IGNORE-RULES is non-nil, rules do not depend on resulting hash."
  (let ((project (copy-porg-project project)))
    (when ignore-rules
      (setf (porg-project-rules project) nil))
    (porg-sha1sum project)))

(cl-defmethod porg-project-resolve (project note)
  "Resolve rule for NOTE from PROJECT."
  (-find
   (lambda (rule)
     (when-let ((match (porg-rule-match rule)))
       (funcall match note)))
   (-filter #'porg-rule-p (porg-project-rules project))))

(cl-defun porg-define (&rest
                       rules
                       &key
                       name
                       root
                       cache-file
                       input
                       describe
                       &allow-other-keys)
  "Define a project and register it in the build system.

NAME is a string, it must be unique for all calls to
`porg-defined', otherwise one project will override another.

ROOT is a directory where the project is built to.

CACHE-FILE is a path to cache file relative to ROOT.

INPUT is a list of notes to build. It can be a function that
returns the list of notes to build.

DESCRIBE is a function that takes a note and returns it's
description. Defaults to `vulpea-note-title'.

RULES is a list of rules describing how to build INPUT to ROOT."
  (let* ((rules (fun-remove-keyword-params rules))
         (project (make-porg-project
                   :name name
                   :root root
                   :cache-file cache-file
                   :input input
                   :describe (or describe #'vulpea-note-title)
                   :rules rules)))
    (if-let ((val (assoc name porg--projects)))
        (setf (cdr val) project)
      (setf porg--projects (cons (cons name project) porg--projects)))))



(cl-defstruct porg-rule
  name
  match
  dependencies
  target
  publish)

(cl-defun porg-rule (&key
                     name
                     match
                     dependencies
                     target
                     publish)
  "Define a rule with NAME.

NAME is a string, it must be unique in the scope of a single project.

MATCH is a predicate on `vulpea-note' that controls which notes
are built using PUBLISH.

DEPENDENCIES is a function that returns a list of dependencies.
When dependency change, matched is considered as modified.

TARGET is a function that takes single matched note and returns
relative location of the output of PUBLISH function.

PUBLISH is a function the defines how publishing happens. It can
be a simple file copy, or something sophisticated. It takes 3
arguments: piece (values computed by `porg-build-input') of a
single matched note, whole input (as calculated by
`porg-build-input') and cache."
  (make-porg-rule
   :name name
   :match match
   :dependencies dependencies
   :target target
   :publish publish))



(cl-defstruct porg-batch-rule
  name
  filter
  target
  publish)

(cl-defun porg-batch-rule (&key
                           name
                           filter
                           target
                           publish)
  "Define a batch rule with NAME.

NAME is a string, it must be unique in the scope of a single project.

FILTER is a predicate on `vulpea-note' that controls which notes
are passed to PUBLISH function.

TARGET is a relative file path, describes output of this rule. It
can be a function that takes list of matched notes.

PUBLISH is a function the defines how publishing happens. It
takes 4 arguments: notes (that were filtered from project input
based on FILTER), target file, input (as calculated by
`porg-build-input') and cache."
  (make-porg-batch-rule
   :name name
   :filter filter
   :target target
   :publish publish))



;; Build cache is stored in `porg-project-cache-file'. In persisted
;; state it's a list, where first element is id of the cached element
;; (usually `vulpea-note-id'), and the rest is cache value of the
;; cached element, which is defined as a property list:
;;
;;   (:hash :update :deps=(:id :hash))
;;
;; When loaded from file into memory, cache is a hash table, where key
;; is id of the cached element and value is is the same property list.

(cl-defun porg-cache-load (file)
  "Load build cache from FILE.

Return a hash table, where key is some string id of the build
element and value its hash."
  (if (file-exists-p file)
      (let* ((xs
              (with-temp-buffer
                (condition-case nil
	                  (progn
	                    (insert-file-contents file)
                      (read (current-buffer)))
	                (error
	                 (message "Could not read cache from %s" file)))))
             (cache (make-hash-table :test 'equal :size (seq-length xs))))
        (--each xs
          (puthash (nth 0 it) (cadr it) cache))
        cache)
    (make-hash-table :test 'equal)))

(cl-defun porg-cache-write (file cache)
  "Write build CACHE to FILE."
  (with-temp-file file
    (let ((xs (--map
               (list it (gethash it cache))
               (hash-table-keys cache)))
          (print-level nil)
	        (print-length nil))
	    (print xs (current-buffer)))))

(cl-defun porg-cache-put (key prop value cache)
  "Put a PROP VALUE pair to plist with KEY in CACHE."
  (puthash
   key
   (plist-put (gethash key cache) prop value)
   cache)
  cache)

(cl-defun porg-cache-get (key prop cache)
  "Get a value by PROP from plist with KEY from CACHE."
  (plist-get (gethash key cache) prop))



;;;###autoload
(defun porg-run (name)
  "Export project with NAME."
  (let ((project (assoc-default name porg--projects)))
    (unless project
      (user-error "Could not find project with named %s" name))
    (porg-log-s "calculating build plan")
    (let ((default-directory (porg-project-root project)))
      (let* ((cache-file (expand-file-name (porg-project-cache-file project)))
             (cache (porg-cache-load cache-file))
             (describe (porg-project-describe project))
             (input (porg-build-input project))
             (notes (-map (-rpartial #'plist-get :note) (hash-table-values input)))
             (plan (porg-build-plan project input cache))
             (build-size (seq-length (plist-get plan :build)))
             (batch-rules (-filter #'porg-batch-rule-p
                                   (porg-project-rules project))))

        (porg-log-s "cleanup")
        (unless (plist-get plan :delete)
          (porg-log "Nothing to delete, everything is used."))

        (porg-log-s "build")
        (unless (plist-get plan :build)
          (porg-log "Nothing to build, everything is up to date."))
        (--each-indexed (plist-get plan :build)
          (let* ((piece (gethash it input))
                 (publish (porg-rule-publish (plist-get piece :rule))))
            (porg-log
             "[%s/%s] building %s"
             (string-from-number (+ 1 it-index) :padding-num build-size)
             build-size
             (funcall describe piece))
            (when publish
              (funcall publish piece input cache))))

        (porg-log-s "run batch actions")
        (unless batch-rules
          (porg-log "No batch rules to run"))
        (--each-indexed batch-rules
          (let* ((filter (porg-batch-rule-filter it))
                 (notes (if filter (funcall #'-filter filter notes) notes))
                 (target (porg-batch-rule-target it))
                 (target (if (functionp target) (funcall target notes) target)))
            (porg-log
             "[%s/%s] running %s batch rule on the set of %s notes"
             (string-from-number (+ 1 it-index) :padding-num (seq-length batch-rules))
             (seq-length batch-rules)
             (porg-batch-rule-name it)
             (seq-length notes))
            (funcall (porg-batch-rule-publish it) notes target input cache)))

        (porg-log-s "cache build files")
        (porg-cache-put (concat "project:" name)
                        :hash (porg-project-hash project 'ignore-rules)
                        cache)
        (--each (-filter #'porg-rule-p (porg-project-rules project))
          (porg-cache-put (concat "rule:" (porg-rule-name it)) :hash (porg-sha1sum it) cache))
        (--each (plist-get plan :build)
          (let ((piece (gethash it input)))
            (porg-cache-put it :hash (plist-get piece :hash) cache)
            (porg-cache-put it :update (format-time-string "%F") cache)
            (porg-cache-put it :deps
                            (-map
                             (lambda (dep)
                               (list
                                :id (plist-get dep :id)
                                :hash (plist-get dep :hash)))
                             (plist-get piece :deps))
                            cache)))
        (porg-cache-write cache-file cache)
        (porg-log "The work is done! Enjoy your published vulpea notes!")
        (porg-log "        ٩(^ᴗ^)۶")))))

(defun porg-build-input (project)
  "Calculate input data for the PROJECT.

Result is a table, where key is note id and the value is a
property list (:note :hash :rule :target :deps=(:id :object :hash))."
  (let* ((describe (porg-project-describe project))
         (input (porg-project-input project))
         (input (if (functionp input) (funcall input) input))
         (size (seq-length input))
         (without-rule nil)
         (tbl (make-hash-table :test 'equal :size size)))

    (porg-log "Found %s notes to resolve." size)

    (--each input
      (if-let ((rule (porg-project-resolve project it)))
          (let ((target (when-let ((target-fn (porg-rule-target rule)))
                          (funcall target-fn it))))
            (puthash
             (vulpea-note-id it)
             (list :note it
                   ;; TODO: this can be taken from DB instead of calculating
                   :hash (porg-sha1sum it)
                   :rule rule
                   :target target
                   :target-rel (when target (s-chop-prefix default-directory target))
                   :deps (when-let* ((deps-fn (porg-rule-dependencies rule))
                                     (deps (funcall deps-fn it)))
                           (--map
                            (list :id (if (vulpea-note-p it)
                                          (vulpea-note-id it)
                                        it)
                                  :object it
                                  :hash (porg-sha1sum it))
                            deps)))
             tbl))
        (setf without-rule (cons it without-rule))))

    ;; quit if not all input can be handled by this project rules
    (when without-rule
      (porg-log "Could not find rule for %s notes:" (seq-length without-rule))
      (--each without-rule
        (porg-log "- %s" (funcall describe it)))
      (user-error "Not all input notes have matching rules, see above"))

    tbl))

(defun porg-build-plan (project input cache)
  "Calculate build plan of INPUT for PROJECT with CACHE.

Result is a property list (:build :delete)."
  (let* ((project-hash (porg-project-hash project 'ignore-rules))
         (project-updated (not (string-equal
                                project-hash
                                (porg-cache-get
                                 (concat "project:" (porg-project-name project))
                                 :hash cache))))
         (build (if project-updated
                    (hash-table-keys input)
                  (-filter
                   (lambda (id)
                     (let* ((piece (gethash id input))
                            (rule (plist-get piece :rule))
                            (deps-cached (porg-cache-get id :deps cache)))
                       (or
                        ;; rule building it changed
                        (not (string-equal
                              (porg-sha1sum rule)
                              (porg-cache-get
                               (concat "rule:" (porg-rule-name rule))
                               :hash cache)))
                        ;; note itself is changed
                        (not (string-equal
                              (plist-get piece :hash)
                              (porg-cache-get id :hash cache)))
                        ;; one of the deps is changed
                        (-any-p
                         (lambda (a)
                           (let ((a-cached (-find
                                            (lambda (x)
                                              (string-equal (plist-get a :id)
                                                            (plist-get x :id)))
                                            deps-cached)))
                             (or (null a-cached)
                                 (not (string-equal (plist-get a :hash)
                                                    (plist-get a-cached :hash))))))
                         (plist-get piece :deps)))))
                   (hash-table-keys input)))))
    (when project-updated
      (porg-log "Project definition has changed, so invalidating cache..."))
    (porg-log "Found %s notes to build." (seq-length build))
    (porg-log "Found %s notes to delete." 0)

    (list
     :build build
     ;; TODO: in order to understand what exactly to delete, we also
     ;; need to cache target files.
     :delete nil)))



;; Publish utilities. Use these functions in your rule definition.

(cl-defun porg-copy-note (note file &key copy-fn)
  "Copy NOTE to FILE using COPY-FN.

Unless COPY-FN is specified, file is simply copied. But you can
control how file is being copied by passing a function that takes
two arguments (note and destination path), which does the copying."
  (let ((dir (file-name-directory file)))
    (mkdir dir 'parents)
    (if copy-fn
        (fun-silent copy-fn note file)
      (copy-file (vulpea-note-path note) file 'replace))))

(cl-defun porg-copy-attachments (note &key dest-fn filter-fn copy-fn)
  "Copy attachments of NOTE to some destination.

DEST-FN can be either a string or a function that takes
attachment name and returns a string. For example, this can be
used to copy attachments to different destinations based on their
type.

FILTER-FN controls how attachments get copied, it's a function that
takes attachment name and returns non-nil if attachment should be
copied. When FILTER-FN is not provided, all attachments are copied.

COPY-FN defaults to `copy-file'."
  (vulpea-utils-with-note note
    (seq-each
     (lambda (link)
       (let ((type (org-ml-get-property :type link))
             (path (org-ml-get-property :path link)))
         (when (and (string-equal type "attachment")
                    (or (not filter-fn) (funcall filter-fn path)))
           (let ((dest (if (functionp dest-fn) (funcall dest-fn path) dest-fn)))
             (mkdir dest 'parents)
             (goto-char (org-ml-get-property :begin link))
             (funcall (or copy-fn #'copy-file)
                      (org-attach-expand path)
                      (expand-file-name path dest)
                      'replace)))))
     (seq-reverse
      (org-element-map (org-element-parse-buffer) 'link #'identity)))))

(cl-defun porg-clean-noexport-headings (file)
  "Remove headings tagged as noexport from FILE."
  (with-current-buffer (find-file-noselect file)
    (seq-each
     (lambda (hl)
       (when (seq-contains-p (org-ml-get-property :tags hl) "noexport")
         (delete-region (org-ml-get-property :begin hl)
                        (org-ml-get-property :end hl))))
     (seq-reverse (org-element-map (org-element-parse-buffer) 'headline #'identity)))
    (save-buffer)))

(cl-defun porg-clean-links-in-buffer (&key sanitize-id-fn sanitize-attachment-fn)
  "Clean links in current buffer.

Each link in the buffer is modified based on its type.

If it's ID link, then it's kept as as link unconditionally.
SANITIZE-ID-FN is called with link to allow custom modifications.

If it's ATTACHMENT link, then it's kept as link unconditionally.
SANITIZE-ATTACHMENT-FN is called with link to allow custom
modifications.

If it's HTTPS link, then it's kept as is without modifications.

All other links are transformed to plain text."
  (-> (seq-reverse (org-element-map (org-element-parse-buffer) 'link #'identity))
      (--each
          (org-ml-update
            (lambda (link)
              (let ((type (org-ml-get-property :type link)))
                (cond
                 ((seq-contains-p '("https") type) link)

                 ((string-equal type "attachment")
                  (if sanitize-attachment-fn (funcall sanitize-attachment-fn link) link))

                 ((string-equal type "id")
                  (if sanitize-id-fn (funcall sanitize-id-fn link) link))

                 (t (org-ml-from-string
                     'plain-text
                     (concat (nth 2 link) (s-repeat (or (org-ml-get-property :post-blank link) 0) " ")))))))
            it))))



;; Logging utilities

(defun porg-log-s (format-string &rest args)
  "Log FORMAT-STRING with ARGS as section.

The output width is limited to 80 characters."
  (message (porg-section (apply #'format format-string args))))

(defun porg-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS.

The output width is limited to 80 characters."
  (message (s-truncate 80 (apply #'format format-string args))))

(defun porg-section (str)
  "Convert STR into nice section."
  (let* ((s (concat ">>> " str))
         (l 76))
    (concat
     "┌──────────────────────────────────────────────────────────────────────────────┐\n"
     "│ " (s-truncate l (s-pad-right l " " s)) " │\n"
     "└──────────────────────────────────────────────────────────────────────────────┘")))



;;  Other utilities

(cl-defun porg-slug (title)
  "Convert TITLE to slug."
  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768  ; U+0300 COMBINING GRAVE ACCENT
                           769  ; U+0301 COMBINING ACUTE ACCENT
                           770  ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771  ; U+0303 COMBINING TILDE
                           772  ; U+0304 COMBINING MACRON
                           774  ; U+0306 COMBINING BREVE
                           775  ; U+0307 COMBINING DOT ABOVE
                           776  ; U+0308 COMBINING DIAERESIS
                           777  ; U+0309 COMBINING HOOK ABOVE
                           778  ; U+030A COMBINING RING ABOVE
                           779  ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780  ; U+030C COMBINING CARON
                           795  ; U+031B COMBINING HORN
                           803  ; U+0323 COMBINING DOT BELOW
                           804  ; U+0324 COMBINING DIAERESIS BELOW
                           805  ; U+0325 COMBINING RING BELOW
                           807  ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                      ("__*" . "_") ;; remove sequential underscores
                      ("^_" . "")   ;; remove starting underscore
                      ("_$" . ""))) ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

(defun porg-sha1sum (obj)
  "Calculate SHA1 sum of OBJ.

OBJ can be either a note, a file or a Lisp object."
  (cond
   ((vulpea-note-p obj)
    (vulpea-utils-note-hash obj))

   ((and
     (stringp obj)
     (file-exists-p obj))
    (shell-command-to-string
     (format "sha1sum '%s' | cut -d ' ' -f 1 -" obj)))

   (t (with-temp-buffer
        (let ((print-level nil)
	            (print-length nil))
	        (print obj (current-buffer))
          (secure-hash 'sha1 (current-buffer)))))))



(provide 'lib-publicatorg)
;;; lib-publicatorg.el ends here
