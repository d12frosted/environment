;;; nucleus-modules.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;; URL: https://github.com/d12frosted/environment/emacs
;;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defvar nucleus-init-modules-p nil
  "Non-nil if `nucleus-initialize-modules' has run.")

(defvar nucleus-modules ()
  "A hash table of enabled modules. Set by `nucleus-initialize-modules'.")

(defvar nucleus-modules-dirs
  (list nucleus-modules-dir)
  "A list of module root directories. Order determines
  priority.")

(defvar nucleus--current-module nil)
(defvar nucleus--current-flags nil)

;;
;; Bootstrap API

(defun nucleus-initialize-modules (&optional force-p)
  "Loads the dna.el in `nucleus-emacs-dir' and sets up hooks for
a healthy session of evolution. Will noop if used more than once,
unless FORCE-P is non-nil."
  (when (or force-p (not nucleus-init-modules-p))
    (setq nucleus-init-modules-p t)

    (load! "dna" nucleus-emacs-dir t)
    (unless nucleus-modules
      (setq nucleus-modules (make-hash-table :test 'equal)))

    (maphash (lambda (key plist)
               (let ((nucleus--current-module key)
                     (nucleus--current-flags (plist-get plist :flags)))
                 (load! "init" (plist-get plist :path) t)))
             nucleus-modules)
    (run-hook-wrapped 'nucleus-init-hook #'nucleus-try-run-hook)
    (unless noninteractive
      (maphash (lambda (key plist)
                 (let ((nucleus--current-module key)
                       (nucleus--current-flags (plist-get plist :flags)))
                   (load! "config" (plist-get plist :path) t)))
               nucleus-modules)
      (load! "config" nucleus-private-dir t)
      (unless custom-file
        (setq custom-file (concat nucleus-local-dir "custom.el")))
      (when (stringp custom-file)
        (load custom-file t t t))
      (run-hook-wrapped 'nucleus-post-init-hook #'nucleus-try-run-hook))))


;;
;; Module API

(defun nucleus-module-p (category module)
  "Returns t if CATEGORY MODULE is enabled (ie. present in
`nucleus-modules')."
  (declare (pure t) (side-effect-free t))
  (and (hash-table-p nucleus-modules)
       (gethash (cons category module) nucleus-modules)
       t))

(defun nucleus-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY,
specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let* ((plist (gethash (cons category module) nucleus-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun nucleus-module-put (category module &rest plist)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be
additional pairs of PROPERTY and VALUEs.

\(fn CATEGORY MODULE PROPERTY VALUE &rest [PROPERTY VALUE [...]])"
  (if-let* ((old-plist (nucleus-module-get category module)))
      (progn
        (when plist
          (when (cl-oddp (length plist))
            (signal 'wrong-number-of-arguments (list (length plist))))
          (while plist
            (plist-put old-plist (pop plist) (pop plist))))
        (puthash (cons category module) old-plist nucleus-modules))
    (puthash (cons category module) plist nucleus-modules)))

(defun nucleus-module-set (category module &rest plist)
  "Enables a module by adding it to `nucleus-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled category flags
  :path  [STRING]       path to category root directory

Example:
  (nucleus-module-set :lang 'haskell :flags '(+intero))"
  (puthash (cons category module)
           plist
           nucleus-modules))

(defun nucleus-module-path (category module &optional file)
  "Like `expand-file-name', but expands FILE relative to CATEGORY (keywordp) and
MODULE (symbol).

If the category isn't enabled this will always return nil. For finding disabled
modules use `nucleus-module-locate-path'."
  (let ((path (nucleus-module-get category module :path))
        file-name-handler-alist)
    (if file (expand-file-name file path)
      path)))

(defun nucleus-module-locate-path (category &optional module file)
  "Searches `nucleus-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path.

This doesn't require modules to be enabled. For enabled modules us
`nucleus-module-path'."
  (when (keywordp category)
    (setq category (nucleus-keyword-name category)))
  (when (and module (symbolp module))
    (setq module (symbol-name module)))
  (cl-loop with file-name-handler-alist = nil
           for default-directory in nucleus-modules-dirs
           for path = (concat category "/" module "/" file)
           if (file-exists-p path)
           return (expand-file-name path)))

(defun nucleus-module-from-path (&optional path)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path)."
  (or nucleus--current-module
      (let* (file-name-handler-alist
             (path (or path (FILE!))))
        (save-match-data
          (setq path (file-truename path))
          (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
            (when-let* ((category (match-string 1 path))
                        (module   (match-string 2 path)))
              (cons (nucleus-keyword-intern category)
                    (intern module))))))))

(defun nucleus-module-load-path (&optional all-p)
  "Return a list of absolute file paths to activated modules. If ALL-P is
non-nil, return paths of possible modules, activated or otherwise."
  (declare (pure t) (side-effect-free t))
  (append (if all-p
              (nucleus-files-in nucleus-modules-dirs
                             :type 'dirs
                             :mindepth 1
                             :depth 1
                             :full t)
            (cl-loop for plist being the hash-values of (nucleus-modules)
                     collect (plist-get plist :path)))
          (list nucleus-private-dir)))

(defun nucleus-modules (&optional refresh-p)
  "Minimally initialize `nucleus-modules' (a hash table) and return it."
  (or (unless refresh-p nucleus-modules)
      (let ((noninteractive t)
            nucleus-modules
            nucleus-init-modules-p)
        (message "Initializing modules")
        (load! "init" nucleus-private-dir t)
        (or nucleus-modules
            (make-hash-table :test 'equal
                             :size 20
                             :rehash-threshold 1.0)))))


;;
;; Use-package modifications

(autoload 'use-package "use-package-core" nil nil t)

(setq use-package-compute-statistics nucleus-debug-mode
      use-package-verbose nucleus-debug-mode
      use-package-minimum-reported-time (if nucleus-debug-mode 0 0.1)
      use-package-expand-minimally (not noninteractive))

;; Adds the :after-call custom keyword to `use-package' (and consequently,
;; `def-package!'). :after-call takes a symbol or list of symbols. These symbols
;; can be functions or hook variables.
;;
;;   (use-package X :after-call find-file-hook)
;;
;; This will load X on the first invokation of `find-file-hook' (then it will
;; remove itself from the hook/function).
(defvar nucleus--deferred-packages-alist '(t))
(after! use-package-core
  (add-to-list 'use-package-deferring-keywords :defer-incrementally nil #'eq)
  (add-to-list 'use-package-deferring-keywords :after-call nil #'eq)

  (setq use-package-keywords
        (use-package-list-insert :defer-incrementally use-package-keywords :after))
  (setq use-package-keywords
        (use-package-list-insert :after-call use-package-keywords :after))

  (defalias 'use-package-normalize/:defer-incrementally 'use-package-normalize-symlist)
  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((nucleus-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            targets)))
     (use-package-process-keywords name rest state)))

  (defalias 'use-package-normalize/:after-call 'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (intern (format "nucleus|transient-hook--load-%s" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (when nucleus-debug-mode
                     (message "Loading deferred package %s from %s" ',name ',fn))
                   (condition-case e (require ',name)
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (dolist (hook (cdr (assq ',name nucleus--deferred-packages-alist)))
                     (if (functionp hook)
                         (advice-remove hook #',fn)
                       (remove-hook hook #',fn)))
                   (delq (assq ',name nucleus--deferred-packages-alist)
                         nucleus--deferred-packages-alist)
                   (fmakunbound ',fn))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (functionp hook)
                       `(advice-add #',hook :before #',fn)
                     `(add-hook ',hook #',fn))
                   forms)))
         `((unless (assq ',name nucleus--deferred-packages-alist)
             (push '(,name) nucleus--deferred-packages-alist))
           (nconc (assq ',name nucleus--deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))


;;
;; Module config macros

(defmacro nucleus! (&rest modules)
  "Bootstraps NUCLEUS Emacs and its modules.

The bootstrap process involves making sure the essential directories exist, core
packages are installed, `nucleus-autoload-file' is loaded, `nucleus-packages-file'
cache exists (and is loaded) and, finally, loads your private init.el (which
should contain your `nucleus!' block).

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

The overall load order of Nucleus is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  `nucleus-pre-init-hook'
  ~/.nucleus.d/init.el
  Module init.el files
  `nucleus-init-hook'
  Module config.el files
  ~/.nucleus.d/config.el
  `after-init-hook'
  `emacs-startup-hook'
  `nucleus-post-init-hook' (at end of `emacs-startup-hook')

Module load order is determined by your `nucleus!' block. See `nucleus-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (unless nucleus-modules
    (setq nucleus-modules
          (make-hash-table :test 'equal
                           :size (if modules (length modules) 150)
                           :rehash-threshold 1.0)))
  (let (category m)
    (while modules
      (setq m (pop modules))
      (cond ((keywordp m) (setq category m))
            ((not category) (error "No module category specified for %s" m))
            ((catch 'nucleus-modules
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (if-let* ((path (nucleus-module-locate-path category module)))
                     (nucleus-module-set category module :flags flags :path path)
                   (message "Warning: couldn't find the %s %s module" category module))))))))
  `(setq nucleus-modules ',nucleus-modules))

(defvar nucleus-disabled-packages)
(defmacro def-package! (name &rest plist)
  "This is a thin wrapper around `use-package'."
  `(use-package ,name
     ,@(if (memq name nucleus-disabled-packages) `(:disabled t))
     ,@plist))

(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

Only use this macro in a module's init.el file.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config

WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Nucleus's config)."
  (declare (indent defun))
  (nucleus--assert-stage-p 'init #'package!)
  (unless (memq when '(:pre-init :post-init :pre-config :post-config))
    (error "'%s' isn't a valid hook for def-package-hook!" when))
  `(progn
     (setq use-package-inject-hooks t)
     (add-hook!
       ',(intern (format "use-package--%s--%s-hook"
                         package
                         (substring (symbol-name when) 1)))
       ,@body)))

(defmacro require! (category module &rest plist)
  "Loads the module specified by CATEGORY (a keyword) and MODULE (a symbol)."
  `(let ((module-path (nucleus-module-locate-path ,category ',module)))
     (nucleus-module-set
      ,category ',module
      ,@(when plist
          (let ((old-plist (nucleus-module-get category module)))
            (unless (plist-member plist :flags)
              (plist-put plist :flags (plist-get old-plist :flags)))
            (unless (plist-member plist :path)
              (plist-put plist :path (or (plist-get old-plist :path)
                                         (nucleus-module-locate-path category module)))))
          plist))
     (if (directory-name-p module-path)
         (condition-case-unless-debug ex
             (let ((nucleus--current-module ',(cons category module)))
               (load! "init" module-path :noerror)
               (let ((nucleus--stage 'config))
                 (load! "config" module-path :noerror)))
           ('error
            (lwarn 'nucleus-modules :error
                   "%s in '%s %s' -> %s"
                   (car ex) ,category ',module
                   (error-message-string ex))))
       (warn 'nucleus-modules :warning "Couldn't find module '%s %s'"
             ,category ',module))))

(defmacro featurep! (category &optional module flag)
  "Returns t if CATEGORY MODULE is enabled. If FLAG is provided, returns t if
CATEGORY MODULE has FLAG enabled.

  (featurep! :config default)

Module FLAGs are set in your config's `nucleus!' block, typically in
~/.emacs.d/init.el. Like so:

  :config (default +flag1 -flag2)

When this macro is used from inside a module, CATEGORY and MODULE can be
omitted. eg. (featurep! +flag1)"
  (and (cond (flag (memq flag (nucleus-module-get category module :flags)))
             (module (nucleus-module-p category module))
             (nucleus--current-flags (memq category nucleus--current-flags))
             ((let ((module-pair
                     (or nucleus--current-module
                         (nucleus-module-from-path (FILE!)))))
                (unless module-pair
                  (error "featurep! couldn't detect what module its in! (in %s)" (FILE!)))
                (memq category (nucleus-module-get (car module-pair) (cdr module-pair) :flags)))))
       t))


;;
;; FIXME Cross-module configuration (deprecated)

;; I needed a way to reliably cross-configure modules without littering my
;; modules with `after!' blocks or testing whether they were enabled, so I wrote
;; `set!'. If a setting doesn't exist at runtime, the `set!' call is ignored and
;; its arguments are left unevaluated (and entirely omitted when byte-compiled).

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting. Like `defmacro', this should return a form to be executed
when called with `set!'. FORMS are not evaluated until `set!' calls it.

See `nucleus/describe-setting' for a list of available settings.

Do not use this for configuring Nucleus core."
  (declare (indent defun) (doc-string 3))
  (or (keywordp keyword)
      (signal 'wrong-type-argument (list 'keywordp keyword)))
  (unless (stringp docstring)
    (push docstring forms)
    (setq docstring nil))
  (let ((alias (plist-get forms :obsolete)))
    (when alias
      (setq forms (plist-put forms :obsolete 'nil)))
    `(fset ',(intern (format "nucleus--set%s" keyword))
           (lambda ,arglist
             ,(if (and (not docstring) (fboundp alias))
                  (documentation alias t)
                docstring)
             ,(when alias
                `(declare (obsolete ,alias "2.1.0")))
             (prog1 (progn ,@forms)
               ,(when alias
                  `(unless noninteractive
                     (message ,(format "The `%s' setting is deprecated, use `%s' instead"
                                       keyword alias)))))))))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if doesn't exist. See
`nucleus/describe-setting' for a list of available settings.

VALUES doesn't get evaluated if the KEYWORD setting doesn't exist."
  (declare (indent defun))
  (let ((fn (intern-soft (format "nucleus--set%s" keyword))))
    (if (and fn (fboundp fn))
        (apply fn values)
      (when (or nucleus-debug-mode after-init-time)
        (message "No setting found for %s" keyword)
        nil))))

(provide 'nucleus-modules)

;;; nucleus-modules.el ends here
