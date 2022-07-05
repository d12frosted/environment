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
;;; Code:

(require 'org-ml)
(require 'vulpea)
(require 'lib-fun)
(require 'lib-vulpea)



(defvar porg--projects nil)

(cl-defgeneric porg-describe (thing)
  "Describe THING.")



(cl-defstruct (porg-project (:constructor porg-project-create)
                            (:copier porg-project-copy))
  (name nil :read-only t :type string)
  (root nil :read-only t :type string)
  (cache-file  nil :read-only t :type string)
  (input nil :read-only t :type function)
  (describe #'porg-describe :read-only t :type function)
  (rules nil :type list)
  (compilers nil :type list))

(cl-defun porg-define (&rest args)
  "Smart constructor for `porg-project'.

ARGS are used to construct project."
  (let* ((project (apply #'porg-project-create args))
         (name (porg-project-name project)))
    (setf (porg-project-compilers project)
          (cons
           (porg-compiler
            :name "$$void$$"
            :match (-rpartial #'porg-rule-output-that :type "$$void$$"))
           (porg-project-compilers project)))
    (if-let ((val (assoc name porg--projects)))
        (setf (cdr val) project)
      (setf porg--projects (cons (cons name project) porg--projects)))
    project))

(cl-defmethod porg-project-hash (project &optional ignore-rules)
  "Calculate hash of the PROJECT.

When IGNORE-RULES is non-nil, rules do not depend on resulting hash."
  (let ((project (porg-project-copy project)))
    (when ignore-rules
      (setf (porg-project-rules project) nil)
      (setf (porg-project-compilers project) nil))
    (porg-sha1sum project)))

(cl-defmethod porg-project-resolve-rule ((project porg-project) note)
  "Resolve rule for NOTE from PROJECT."
  (-find
   (lambda (rule)
     (when-let ((match (porg-rule-match rule)))
       (funcall match note)))
   (-filter #'porg-rule-p (porg-project-rules project))))

(cl-defmethod porg-project-resolve-compiler ((project porg-project) output)
  "Resolve compiler for OUTPUT from PROJECT."
  (-find
   (lambda (compiler)
     (when-let ((match (porg-compiler-match compiler)))
       (funcall match output)))
   (-filter #'porg-compiler-p (porg-project-compilers project))))



(cl-defstruct (porg-compiler (:constructor porg-compiler)
                             (:copier nil))
  "Define a compiler for `porg-rule-output'.

MATCH is a predicate on `porg-rule-output' that control which
items are build using this rule."
  (name nil :read-only t :type string)
  (match nil :read-only t :type function)
  (build nil :read-only t :type function)
  (clean nil :read-only t :type function)
  (hash nil :read-only t :type function))

(cl-defstruct (porg-rule (:constructor porg-rule)
                         (:copier nil))
  "Define a rule.

NAME is a string, it must be unique in the scope of a single project.

MATCH is a predicate on `vulpea-note' that controls which notes
are built using this rule.

OUTPUTS is a function that takes single matched note and returns
list of `porg-rule-output'."
  (name nil :read-only t :type string)
  (match nil :read-only t :type function)
  (outputs nil :read-only t :type function))



(cl-defstruct (porg-rule-output (:constructor porg-rule-output)
                                (:copier nil))
  "Define a `porg-rule' output.

ID is a string identifier of the output item.

TYPE is a string representing type of the ITEM. Publicatorg uses
the TYPE to find suitable compiler. For example, TYPE can be
note, attachment, etc.

ITEM is an object that needs to be built.

FILE is the relative location of the output.

HARD-DEPS and SOFT-DEPS are lists of dependencies. When
dependency change, matched is considered as modified. Hard
dependency is strictly required for rule to succeed. Use
SOFT-DEPS when you simply want to make sure that this rule needs
to run again whenever soft dependency change. Use HARD-DEPS when
you want compilation to fail if dependency is missing.

Difference between hard and soft dependencies is that hard
dependencies declare what notes are required to build this rule."
  (id nil :read-only t :type string)
  (type nil :read-only t :type string)
  (item nil :read-only t)
  (file nil :read-only t :type string)
  (hard-deps nil :read-only t :type function)
  (soft-deps nil :read-only t :type function))

(cl-defun porg-note-output (note &key file soft-deps hard-deps)
  "Make an output for NOTE.

See `porg-rule-output' for explanation of FILE, SOFT-DEPS, and HARD-DEPS.

In addition, SOFT-DEPS are concatenated with list all linked notes."
  (porg-rule-output
   :id (vulpea-note-id note)
   :type "note"
   :item note
   :file file
   :hard-deps hard-deps
   :soft-deps
   (-concat soft-deps
            (->> (vulpea-note-links note)
                 (--filter (string-equal "id" (car it)))
                 (--map (cdr it))))))

(cl-defun porg-attachments-output (note &key dir file-mod filter owner)
  "Make an list of attachments output for NOTE.

DIR can be either a string or a function that takes
attachment name and returns a string. For example, this can be
used to copy attachments to different destinations based on their
type.

FILE-MOD allows to modify output file name.

FILTER controls which attachments get copied, it's a function that
takes attachment name and returns non-nil if attachment should be
copied. When FILTER-FN is not provided, all attachments are copied.

OWNER allows to steal attachments of one NOTE to another OWNER."
  (vulpea-utils-with-note note
    (->>
     (seq-reverse (org-element-map (org-element-parse-buffer) 'link #'identity))
     (--filter
      (and (string-equal (org-ml-get-property :type it) "attachment")
           (or (not filter) (funcall filter (org-ml-get-property :path it)))))
     (--map
      (let* ((path (org-ml-get-property :path it))
             (dir (if (functionp dir) (funcall dir path) dir))
             (newname (concat (file-name-as-directory dir) path))
             (newname (if file-mod (funcall file-mod newname) newname)))
        (goto-char (org-ml-get-property :begin it))
        (porg-rule-output
         :id (concat (vulpea-note-id (or owner note)) ":" path)
         :type "attachment"
         :item (org-attach-expand path)
         :file newname))))))

(cl-defun porg-void-output (note)
  "Make a void output for NOTE."
  (porg-rule-output
   :id (vulpea-note-id note)
   :type "$$void$$"
   :item note))

(cl-defmethod porg-rule-output-that ((output porg-rule-output) &key type predicate)
  "Check that OUTPUT has TYPE and satisfies PREDICATE (optional)."
  (and (string-equal (porg-rule-output-type output) type)
       (or (not predicate) (funcall predicate (porg-rule-output-item output)))))



(cl-defstruct (porg-batch-rule (:constructor porg-batch-rule)
                               (:copier nil))
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
  name
  filter
  target
  publish)



(cl-defstruct (porg-cache-item (:constructor porg-cache-item-create))
  (hash nil :type string)
  (output nil :type string)
  (rule nil :type string)
  (compiler nil :type string))

(cl-defun porg-cache-query (cache id access)
  "Query CACHE for ID by ACCESS."
  (when-let ((o (gethash id cache)))
    (funcall access o)))

(cl-defun porg-cache-load (file)
  "Load build cache from FILE.

Return a hash table, where key is some string id of the build
element and value its hash."
  (if (file-exists-p file)
      (with-temp-buffer
        (condition-case nil
	          (progn
	            (insert-file-contents file)
              (read (current-buffer)))
	        (error
	         (message "Could not read cache from %s" file))))
    (make-hash-table :test 'equal)))

(cl-defun porg-cache-write (file cache)
  "Write build CACHE to FILE."
  (with-temp-file file
    (let ((print-level nil)
	        (print-length nil))
	    (print cache (current-buffer)))))



;;;###autoload
(defun porg-run (name)
  "Export project with NAME."
  (let ((project (assoc-default name porg--projects)))
    (unless project
      (user-error "Could not find project named '%s'" name))
    (porg-log-s "calculating build plan")
    (let ((default-directory (porg-project-root project)))
      (let* ((cache-file (expand-file-name (porg-project-cache-file project)))
             (cache (porg-cache-load cache-file))
             (describe (porg-project-describe project))
             (items (porg-build-input project cache))
             (plan (porg-build-plan project items cache))
             (build-size (seq-length (plist-get plan :build)))
             (delete-size (seq-length (plist-get plan :delete)))
             (batch-rules (-filter #'porg-batch-rule-p
                                   (porg-project-rules project))))

        (porg-log-s "cleanup")
        (unless (plist-get plan :delete)
          (porg-log "Nothing to delete, everything is used."))
        (--each-indexed (plist-get plan :delete)
          (let* ((cached (gethash it cache))
                 (compiler-name (porg-cache-item-compiler cached))
                 (compiler (--find (string-equal compiler-name (porg-compiler-name it))
                                   (porg-project-compilers project))))
            (porg-log
             "[%s/%s] cleaning %s using %s rule from %s"
             (string-from-number (+ 1 it-index) :padding-num delete-size)
             delete-size
             it
             compiler-name
             (porg-cache-item-output cached))
            (funcall (porg-compiler-clean compiler) cached (porg-project-root project))))

        (porg-log-s "build")
        (unless (plist-get plan :build)
          (porg-log "Nothing to build, everything is up to date."))
        (--each-indexed (plist-get plan :build)
          (let ((item (gethash it items)))
            (porg-log
             "[%s/%s] building %s"
             (string-from-number (+ 1 it-index) :padding-num build-size)
             build-size
             (funcall describe item))
            (when-let ((build (porg-compiler-build (porg-item-compiler item))))
              (funcall build item items cache))))

        (porg-log-s "run batch actions")
        (unless batch-rules
          (porg-log "No batch actions to run"))
        (--each-indexed batch-rules
          (let* ((filter (porg-batch-rule-filter it))
                 (items-selected (hash-table-values items))
                 (items-selected (if filter (funcall #'-filter filter items-selected) items-selected))
                 (size (seq-length items-selected))
                 (items-selected (let ((tbl (make-hash-table :test 'equal :size size)))
                                   (--each items-selected (puthash (porg-item-id it) it tbl))
                                   tbl))
                 (target (porg-batch-rule-target it))
                 (target (if (functionp target) (funcall target items) target)))
            (porg-log
             "[%s/%s] running %s batch action on the set of %s notes"
             (string-from-number (+ 1 it-index) :padding-num (seq-length batch-rules))
             (seq-length batch-rules)
             (porg-batch-rule-name it)
             size)
            (funcall (porg-batch-rule-publish it) target items-selected items cache)))

        (porg-log-s "cache build files")
        (puthash (concat "project:" name)
                 (porg-cache-item-create :hash (porg-project-hash project 'ignore-rules))
                 cache)
        (--each (-filter #'porg-rule-p (porg-project-rules project))
          (puthash (concat "rule:" (porg-rule-name it))
                   (porg-cache-item-create :hash (porg-sha1sum it))
                   cache))
        (--each (porg-project-compilers project)
          (puthash (concat "compiler:" (porg-compiler-name it))
                   (porg-cache-item-create :hash (porg-sha1sum it))
                   cache))
        (--each (plist-get plan :delete)
          (remhash it cache))
        (--each (plist-get plan :build)
          (let* ((item (gethash it items)))
            (puthash (porg-item-id item)
                     (porg-cache-item-create
                      :hash (porg-item-hash item)
                      :output (porg-item-target-rel item)
                      :rule (porg-rule-name (porg-item-rule item))
                      :compiler (porg-compiler-name (porg-item-compiler item)))
                     cache)))
        (porg-cache-write cache-file cache)

        (porg-log "The work is done! Enjoy your published vulpea notes!")
        (porg-log "        ٩(^ᴗ^)۶")))))



(cl-defstruct (porg-item (:constructor porg-item-create)
                         (:copier nil))
  id
  type
  item
  hash
  rule
  compiler
  target-abs
  target-rel
  target-hash
  soft-deps
  hard-deps)

(cl-defmethod porg-item-deps ((item porg-item))
  "Return dependencies of ITEM."
  (let ((hard-deps (porg-item-hard-deps item))
        (soft-deps (porg-item-soft-deps item))
        (-compare-fn #'string-equal))
    (-distinct (-concat hard-deps soft-deps))))

(cl-defmethod porg-item-that ((item porg-item) &key type predicate)
  "Check that ITEM has TYPE and satisfies PREDICATE (optional)."
  (and (string-equal (porg-item-type item) type)
       (or (not predicate) (funcall predicate (porg-item-item item)))))



(defun porg-build-input (project cache)
  "Calculate input data for the PROJECT with CACHE.

Result is a table, where key is note id and the value is `porg-item'.

Throws a user error if any of the input has no matching rule."
  (let* ((describe (porg-project-describe project))
         (input (porg-project-input project))
         (input (if (functionp input) (funcall input) input))
         (size (seq-length input))
         (without-rule nil)
         (without-compiler nil)
         (tbl (make-hash-table :test 'equal :size size)))

    (porg-log "Found %s notes to resolve." size)

    (--each input
      (if-let ((rule (porg-project-resolve-rule project it)))
          (--each (when-let ((outputs-fn (porg-rule-outputs rule)))
                    (funcall outputs-fn it))
            (if-let ((compiler (porg-project-resolve-compiler project it)))
                (let* ((target-rel (porg-rule-output-file it))
                       (target-abs (when target-rel
                                     (expand-file-name target-rel (porg-project-root project)))))
                  (progn
                    (puthash
                     (porg-rule-output-id it)
                     (porg-item-create
                      :id (porg-rule-output-id it)
                      :type (porg-rule-output-type it)
                      :item (porg-rule-output-item it)
                      :hash (funcall (or (porg-compiler-hash compiler)
                                         #'porg-sha1sum)
                                     it)
                      :rule rule
                      :compiler compiler
                      :target-abs target-abs
                      :target-rel target-rel
                      :target-hash (when (and target-abs (file-exists-p target-abs))
                                     (or
                                      (porg-cache-query cache (porg-rule-output-id it)
                                                        #'porg-cache-item-hash)
                                      (porg-sha1sum target-abs)))
                      :hard-deps (--map (if (vulpea-note-p it) (vulpea-note-id it) it)
                                        (porg-rule-output-hard-deps it))
                      :soft-deps (--map (if (vulpea-note-p it) (vulpea-note-id it) it)
                                        (porg-rule-output-soft-deps it)))
                     tbl)))
              (setf without-compiler (cons it without-compiler))))
        (setf without-rule (cons it without-rule))))

    (message "Found %s items to resolve" (seq-length (hash-table-keys tbl)))

    ;; quit if not all input can be handled by this project rules or compilers
    (when without-rule
      (porg-log "Could not find rule for %s notes:" (seq-length without-rule))
      (--each without-rule
        (porg-log "- %s" (funcall describe it)))
      (user-error "Not all input notes have matching rules, see above"))
    (when without-compiler
      (porg-log "Could not find compiler for %s items:" (seq-length without-compiler))
      (--each without-compiler
        (porg-log "- %s" (funcall describe it)))
      (user-error "Not all items have matching compilers, see above"))

    (when-let ((missing (--filter
                         (--remove (gethash it tbl) (porg-item-hard-deps it))
                         (hash-table-values tbl))))
      (porg-log "Could not find dependencies for %s items:" (seq-length missing))
      (-each missing
        (lambda (item)
          (--each (--filter (gethash it tbl) (porg-item-hard-deps item))
            (porg-log "Missing hard dependency of '%s': '%s'" (porg-item-id item) it))))
      (user-error "Missing some hard dependencies, see above"))

    tbl))



(defun porg-build-plan (project items cache)
  "Calculate build plan of ITEMS for PROJECT with CACHE.

Result is a property list (:compile :delete)."
  (let* ((project-hash (porg-project-hash project 'ignore-rules))
         (project-updated (not (string-equal
                                project-hash
                                (porg-cache-query
                                 cache (concat "project:" (porg-project-name project))
                                 #'porg-cache-item-hash))))
         (build
          (if project-updated
              (hash-table-keys items)
            (-filter
             (lambda (id)
               (let* ((item (gethash id items))
                      (rule (porg-item-rule item))
                      (compiler (porg-item-compiler item)))
                 (or
                  ;; rule changed
                  (let ((res (not
                              (string-equal
                               (porg-sha1sum rule)
                               (porg-cache-query
                                cache (concat "rule:" (porg-rule-name rule))
                                #'porg-cache-item-hash)))))
                    (when res (porg-debug "%s: rule %s changed"
                                          (porg-describe item)
                                          (porg-rule-name rule)))
                    res)
                  ;; compiler changed
                  (let ((res (not
                              (string-equal
                               (porg-sha1sum compiler)
                               (porg-cache-query
                                cache (concat "compiler:" (porg-compiler-name compiler))
                                #'porg-cache-item-hash)))))
                    (when res (porg-debug "%s: compiler %s changed"
                                          (porg-describe item)
                                          (porg-compiler-name compiler)))
                    res)
                  ;; item itself is changed
                  (let ((res (not
                              (string-equal
                               (porg-item-hash item)
                               (porg-cache-query cache id #'porg-cache-item-hash)))))
                    (when res (porg-debug "%s: content changed" (porg-describe item)))
                    res)

                  ;; one of the deps is changed
                  (-any-p
                   (lambda (a-id)
                     (let ((a (gethash a-id items)))
                       ;; soft deps can be missing
                       (let ((res (and (not (null a))
                                       (not (string-equal (porg-item-hash a)
                                                          (porg-cache-query cache a-id #'porg-cache-item-hash))))))
                         (when res
                           (porg-debug "%s: dependency %s changed"
                                       (porg-describe item)
                                       (if a (porg-describe a) a-id)))
                         res)))
                   (porg-item-deps item)))))
             (hash-table-keys items))))
         (delete (--remove
                  (or (gethash it items)
                      (s-prefix-p "project:" it)
                      (s-prefix-p "rule:" it)
                      (s-prefix-p "compiler:" it))
                  (hash-table-keys cache))))

    (porg-log "Found %s items to compile." (seq-length build))
    (porg-log "Found %s items to delete." (seq-length delete))

    (list
     :build build
     :delete delete)))



;; Publish utilities. Use these functions in your rule definition.

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

(defvar porg-log-level 'info)

(defun porg-log-s (format-string &rest args)
  "Log FORMAT-STRING with ARGS as section.

The output width is limited to 80 characters."
  (message (porg-section (apply #'format format-string args))))

(defun porg-log (format-string &rest args)
  "Log FORMAT-STRING with ARGS.

The output width is limited to 80 characters."
  (message (s-truncate 80 (apply #'format format-string args))))

(defun porg-debug (format-string &rest args)
  "Debug log FORMAT-STRING with ARGS.

Noops depending on `porg-log-level'."
  (when (equal porg-log-level 'debug)
    (message (apply #'format format-string args))))

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
    (caar
     (org-roam-db-query
      [:select hash
       :from files
       :where (= file $s1)]
      (vulpea-note-path obj))))

   ((porg-rule-output-p obj)
    (porg-sha1sum (porg-rule-output-item obj)))

   ((and
     (stringp obj)
     (file-exists-p obj))
    (s-trim
     (shell-command-to-string
      (format "sha1sum '%s' | cut -d ' ' -f 1 -" obj))))

   (t (with-temp-buffer
        (let ((print-level nil)
	            (print-length nil))
	        (print obj (current-buffer))
          (secure-hash 'sha1 (current-buffer)))))))



(provide 'lib-publicatorg)
;;; lib-publicatorg.el ends here
