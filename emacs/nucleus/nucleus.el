;;; nucleus.el --- the heart of the beast -*- lexical-binding: t; -*-

(eval-when-compile
  (and (version< emacs-version "25")
       (error "Detected Emacs %s. Doom only supports Emacs 25.1 and higher"
              emacs-version)))

(defvar nucleus-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all nucleus functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")


;;
;; Constants

(defconst nucleus-version "2.0.9"
  "Current version of DOOM emacs.")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst EMACS27+ (> emacs-major-version 26))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defconst nucleus-projects-dir (concat user-home-directory "Developer/")
  "Directory for personal projects.")

(defvar nucleus-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs directory. Must end in a slash.")

;; TODO remove me
(defvar nucleus-docs-dir (concat nucleus-emacs-dir "doc/"))

(defvar nucleus-core-dir (concat nucleus-emacs-dir "nucleus/")
  "The root directory of Nucleus files.")

(defvar nucleus-modules-dir (concat nucleus-emacs-dir "modules/")
  "The root directory for modules.")

;; TODO: move outside of $XDG_CONFIG_HOME
(defvar nucleus-local-dir (concat nucleus-emacs-dir ".local/")
  "The root directory for local Emacs files.

Use this as permanent storage for files that are safe to share
across systems.")

(defvar nucleus-etc-dir (concat nucleus-local-dir "etc/")
  "Directory for non-volatile storage.

Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defvar nucleus-cache-dir (concat nucleus-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defvar nucleus-packages-dir (concat nucleus-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.")

(defvar nucleus-autoload-file (concat nucleus-local-dir "autoloads.el")
  "Where `nucleus-reload-nucleus-autoloads' will generate its
  core autoloads file.")

(defvar nucleus-package-autoload-file (concat nucleus-local-dir "autoloads.pkg.el")
  "Where `nucleus-reload-package-autoloads' will generate its
package.el autoloads file.")


;;
;; Doom core variables

(defvar nucleus-init-p nil
  "Non-nil if `nucleus-initialize' has run.")

(defvar nucleus-init-time nil
  "The time it took, in seconds, for DOOM Emacs to initialize.")

(defvar nucleus-emacs-changed-p nil
  "If non-nil, the running version of Emacs is different from the first time
Doom was setup, which can cause problems.")

(defvar nucleus-site-load-path load-path
  "The starting load-path, before it is altered by `nucleus-initialize'.")

(defvar nucleus--last-emacs-file (concat nucleus-local-dir "emacs-version.el"))
(defvar nucleus--last-emacs-version nil)
(defvar nucleus--refreshed-p nil)
(defvar nucleus--stage 'init)


;;
;; Custom error types

(define-error 'nucleus-error "Error in Doom Emacs core")
(define-error 'nucleus-hook-error "Error in a Doom startup hook" 'nucleus-error)
(define-error 'nucleus-autoload-error "Error in an autoloads file" 'nucleus-error)
(define-error 'nucleus-module-error "Error in a Doom module" 'nucleus-error)
(define-error 'nucleus-private-error "Error in private config" 'nucleus-error)
(define-error 'nucleus-package-error "Error with packages" 'nucleus-error)


;;
;; Custom hooks

(defvar nucleus-init-hook nil
  "Hooks run after all init.el files are loaded, including your private and all
module init.el files, but before their config.el files are loaded.")

(defvar nucleus-post-init-hook nil
  "A list of hooks run when Doom is fully initialized. Fires near the end of
`emacs-startup-hook', as late as possible. Guaranteed to run after everything
else (except for `window-setup-hook').")

(defvar nucleus-reload-hook nil
  "A list of hooks to run when `nucleus/reload' is called.")

(defvar nucleus-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or reloaded with
`nucleus/reload-theme'.")

(defvar nucleus-exit-window-hook nil
  "Hook run before `switch-window' or `switch-frame' are called.

Also see `nucleus-enter-window-hook'.")

(defvar nucleus-enter-window-hook nil
  "Hook run after `switch-window' or `switch-frame' are called.

Also see `nucleus-exit-window-hook'.")

(defvar nucleus-exit-buffer-hook nil
  "Hook run after `switch-to-buffer', `pop-to-buffer' or `display-buffer' are
called. The buffer to be switched to is current when these hooks run.

Also see `nucleus-enter-buffer-hook'.")

(defvar nucleus-enter-buffer-hook nil
  "Hook run before `switch-to-buffer', `pop-to-buffer' or `display-buffer' are
called. The buffer to be switched to is current when these hooks run.

Also see `nucleus-exit-buffer-hook'.")

(defvar nucleus-inhibit-switch-buffer-hooks nil
  "Letvar for inhibiting `nucleus-enter-buffer-hook' and `nucleus-exit-buffer-hook'.
Do not set this directly.")
(defvar nucleus-inhibit-switch-window-hooks nil
  "Letvar for inhibiting `nucleus-enter-window-hook' and `nucleus-exit-window-hook'.
Do not set this directly.")

(defun nucleus*switch-window-hooks (orig-fn window &optional norecord)
  (if (or nucleus-inhibit-switch-window-hooks
          (null window)
          (eq window (selected-window))
          (window-minibuffer-p)
          (window-minibuffer-p window))
      (funcall orig-fn window norecord)
    (let ((nucleus-inhibit-switch-window-hooks t))
      (run-hooks 'nucleus-exit-window-hook)
      (prog1 (funcall orig-fn window norecord)
        (with-selected-window window
          (run-hooks 'nucleus-enter-window-hook))))))

(defun nucleus*switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (if (or nucleus-inhibit-switch-buffer-hooks
          (eq (get-buffer buffer-or-name) (current-buffer)))
      (apply orig-fn buffer-or-name args)
    (let ((nucleus-inhibit-switch-buffer-hooks t))
      (run-hooks 'nucleus-exit-buffer-hook)
      (prog1 (apply orig-fn buffer-or-name args)
        (when (buffer-live-p (get-buffer buffer-or-name))
          (with-current-buffer buffer-or-name
            (run-hooks 'nucleus-enter-buffer-hook)))))))

(defun nucleus|init-switch-hooks (&optional disable)
  "Set up enter/exit hooks for windows and buffers.

See `nucleus-enter-buffer-hook', `nucleus-enter-window-hook', `nucleus-exit-buffer-hook'
and `nucleus-exit-window-hook'."
  (dolist (spec '((select-window . nucleus*switch-window-hooks)
                  (switch-to-buffer . nucleus*switch-buffer-hooks)
                  (display-buffer . nucleus*switch-buffer-hooks)
                  (pop-to-buffer . nucleus*switch-buffer-hooks)))
    (if disable
        (advice-remove (car spec) (cdr spec))
      (advice-add (car spec) :around (cdr spec)))))

(defun nucleus*load-theme-hooks (theme &rest _)
  "Set up `nucleus-load-theme-hook' to run after `load-theme' is called."
  (setq nucleus-theme theme)
  (run-hooks 'nucleus-load-theme-hook))
(advice-add #'load-theme :after #'nucleus*load-theme-hooks)


;;
;; Emacs core configuration

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 auto-mode-case-fold nil
 autoload-compute-prefixes nil
 debug-on-error nucleus-debug-mode
 ffap-machine-p-known 'reject     ; don't ping things that look like domain names
 idle-update-delay 2              ; update ui less often
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ;; byte compilation
 byte-compile-verbose nucleus-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 ;; security
 gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
 tls-checktrust gnutls-verify-error
 tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                   ;; compatibility fallbacks
                   "gnutls-cli -p %p %h"
                   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
 ;; files
 abbrev-file-name             (concat nucleus-local-dir "abbrev.el")
 auto-save-list-file-name     (concat nucleus-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat nucleus-cache-dir "backup/")))
 mc/list-file                 (concat nucleus-etc-dir "mc-lists.el")
 pcache-directory             (concat nucleus-cache-dir "pcache/")
 request-storage-directory    (concat nucleus-cache-dir "request")
 server-auth-dir              (concat nucleus-cache-dir "server/")
 shared-game-score-directory  (concat nucleus-etc-dir "shared-game-score/")
 tramp-auto-save-directory    (concat nucleus-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat nucleus-cache-dir "tramp-persistency.el")
 url-cache-directory          (concat nucleus-cache-dir "url/")
 url-configuration-directory  (concat nucleus-etc-dir "url/")
 gamegrid-user-score-file-directory (concat nucleus-etc-dir "games/"))

(defvar nucleus-auto-minor-mode-alist '()
  "Alist mapping filename patterns to corresponding minor mode functions, like
`auto-mode-alist'. All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defun nucleus|enable-minor-mode-maybe ()
  "Check file name against `nucleus-auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist nucleus-auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))
(add-hook 'find-file-hook #'nucleus|enable-minor-mode-maybe)

(defun nucleus*symbol-file (orig-fn symbol &optional type)
  "If a `nucleus-file' symbol property exists on SYMBOL, use that instead of the
original value of `symbol-file'."
  (or (if (symbolp symbol) (get symbol 'nucleus-file))
      (funcall orig-fn symbol type)))
(advice-add #'symbol-file :around #'nucleus*symbol-file)

;; To speed up minibuffer commands (like helm and ivy), defer garbage collection
;; when the minibuffer is active. It may mean a pause when finished, but that's
;; acceptable instead of pauses during.
(defun nucleus|defer-garbage-collection ()
  (setq gc-cons-threshold nucleus-gc-cons-upper-limit))
(defun nucleus|restore-garbage-collection ()
  (setq gc-cons-threshold nucleus-gc-cons-threshold))
(add-hook 'minibuffer-setup-hook #'nucleus|defer-garbage-collection)
(add-hook 'minibuffer-exit-hook  #'nucleus|restore-garbage-collection)

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defun nucleus|run-local-var-hooks ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
                    #'nucleus-try-run-hook))
(add-hook 'hack-local-variables-hook #'nucleus|run-local-var-hooks)


;;
;; Incremental lazy-loading

(defvar nucleus-incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and can be broken up into:

  (nucleus-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

This is already done by the lang/org module, however.

If you want to disable incremental loading altogether, either remove
`nucleus|load-packages-incrementally' from `emacs-startup-hook' or set
`nucleus-incremental-first-idle-timer' to nil.")

(defvar nucleus-incremental-first-idle-timer 2
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading.")

(defvar nucleus-incremental-idle-timer 1.5
  "How long (in idle seconds) in between incrementally loading packages.")

(defun nucleus-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally, in `nucleus-incremental-idle-timer'
intervals."
  (if (not now)
      (nconc nucleus-incremental-packages packages)
    (when packages
      (let ((gc-cons-threshold nucleus-gc-cons-upper-limit)
            file-name-handler-alist)
        (let* ((reqs (cl-delete-if #'featurep packages))
               (req (ignore-errors (pop reqs))))
          (when req
            (when nucleus-debug-mode
              (message "Incrementally loading %s" req))
            (unless (require req nil t)
              (message "Failed to load '%s' package incrementally" req))
            (when reqs
              (run-with-idle-timer nucleus-incremental-idle-timer
                                   nil #'nucleus-load-packages-incrementally
                                   reqs t))))))))

(defun nucleus|load-packages-incrementally ()
  "Begin incrementally loading packages in `nucleus-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (if (daemonp)
      (mapc #'require (cdr nucleus-incremental-packages))
    (when (integerp nucleus-incremental-first-idle-timer)
      (run-with-idle-timer nucleus-incremental-first-idle-timer
                           nil #'nucleus-load-packages-incrementally
                           (cdr nucleus-incremental-packages) t))))

(add-hook 'emacs-startup-hook #'nucleus|load-packages-incrementally)


;;
;; Bootstrap helpers

(defun nucleus-try-run-hook (hook)
  "Run HOOK (a hook function), but handle errors better, to make debugging
issues easier.

Meant to be used with `run-hook-wrapped'."
  (when nucleus-debug-mode
    (message "Running nucleus hook: %s" hook))
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'nucleus-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun nucleus-ensure-same-emacs-version-p ()
  "Check if the running version of Emacs has changed and set
`nucleus-emacs-changed-p' if it has."
  (if (load nucleus--last-emacs-file 'noerror 'nomessage 'nosuffix)
      (setq nucleus-emacs-changed-p
            (not (equal emacs-version nucleus--last-emacs-version)))
    (with-temp-file nucleus--last-emacs-file
      (princ `(setq nucleus--last-emacs-version ,(prin1-to-string emacs-version))
             (current-buffer))))
  (cond ((not nucleus-emacs-changed-p))
        ((y-or-n-p
          (format
           (concat "Your version of Emacs has changed from %s to %s, which may cause incompatibility\n"
                   "issues. If you run into errors, run `bin/nucleus compile :plugins` or reinstall your\n"
                   "plugins to resolve them.\n\n"
                   "Continue?")
           nucleus--last-emacs-version
           emacs-version))
         (delete-file nucleus--last-emacs-file))
        (noninteractive (error "Aborting"))
        ((kill-emacs))))

(defun nucleus-ensure-core-directories-exist ()
  "Make sure all Doom's essential local directories (in and including
`nucleus-local-dir') exist."
  (dolist (dir (list nucleus-local-dir nucleus-etc-dir nucleus-cache-dir nucleus-packages-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun nucleus|display-benchmark (&optional return-p)
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Doom loaded %s packages across %d modules in %.03fs"
           (length package-activated-list)
           (if nucleus-modules (hash-table-count nucleus-modules) 0)
           (or nucleus-init-time
               (setq nucleus-init-time (float-time (time-subtract (current-time) before-init-time))))))

(defun nucleus|run-all-startup-hooks ()
  "Run all startup Emacs hooks. Meant to be executed after starting Emacs with
-q or -Q, for example:

  emacs -Q -l init.el -f nucleus|run-all-startup-hooks"
  (run-hook-wrapped 'after-init-hook #'nucleus-try-run-hook)
  (setq after-init-time (current-time))
  (dolist (hook (list 'delayed-warnings-hook
                      'emacs-startup-hook 'term-setup-hook
                      'window-setup-hook))
    (run-hook-wrapped hook #'nucleus-try-run-hook)))


;;
;; Bootstrap functions

(defun nucleus-initialize (&optional force-p force-load-core-p)
  "Bootstrap Doom, if it hasn't already (or if FORCE-P is non-nil).

Loads Doom core files if in an interactive session or FORCE-LOAD-CORE-P is
non-nil.

The bootstrap process involves making sure 1) the essential directories exist,
2) the core packages are installed, 3) `nucleus-autoload-file' and
`nucleus-package-autoload-file' exist and have been loaded, and 4) Doom's core
files are loaded.

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

The overall load order of Doom is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  ~/.nucleus.d/init.el
  Module init.el files
  `nucleus-init-hook'
  Module config.el files
  ~/.nucleus.d/config.el
  `nucleus-post-init-hook'
  `after-init-hook'
  `emacs-startup-hook'

Module load order is determined by your `nucleus!' block. See `nucleus-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (when (or force-p (not nucleus-init-p))
    (setq nucleus-init-p t)  ; Prevent infinite recursion

    ;; `nucleus-autoload-file' tells Emacs where to load all its autoloaded
    ;; functions from. This includes everything in core/autoload/*.el and all
    ;; the autoload files in your enabled modules.
    (when (or force-p (not (nucleus-initialize-autoloads nucleus-autoload-file)))
      (nucleus-ensure-core-directories-exist)
      (nucleus-ensure-same-emacs-version-p)

      (require 'nucleus-packages)
      (nucleus-ensure-packages-initialized force-p)
      (nucleus-ensure-core-packages)

      (unless (or force-p noninteractive)
        (user-error "Your nucleus autoloads are missing! Run `bin/nucleus refresh' to regenerate them")))

    ;; Loads `nucleus-package-autoload-file', which loads a concatenated package
    ;; autoloads file and caches `load-path', `auto-mode-alist',
    ;; `Info-directory-list', `nucleus-disabled-packages' and
    ;; `package-activated-list'. A big reduction in startup time.
    (unless (or force-p
                (nucleus-initialize-autoloads nucleus-package-autoload-file)
                noninteractive)
      (user-error "Your package autoloads are missing! Run `bin/nucleus refresh' to regenerate them")))

  (require 'nucleus-os)
  (when (or force-load-core-p (not noninteractive))
    (add-hook! 'emacs-startup-hook
      #'(nucleus|init-switch-hooks nucleus|display-benchmark))

    (require 'nucleus-ui)
    (require 'nucleus-editor)
    (require 'nucleus-projects)
    (require 'nucleus-keybinds)))

(defun nucleus-initialize-autoloads (file)
  "Tries to load FILE (an autoloads file). Return t on success, throws an error
in interactive sessions, nil otherwise (but logs a warning)."
  (condition-case e
      (load (file-name-sans-extension file) 'noerror 'nomessage)
    ((debug error)
     (if noninteractive
         (message "Autoload file warning: %s -> %s" (car e) (error-message-string e))
       (signal 'nucleus-autoload-error (list (file-name-nondirectory file) e))))))


;;
;; Bootstrap Doom

(add-to-list 'load-path nucleus-core-dir)

(require 'nucleus-lib)
(require 'nucleus-modules)
(when noninteractive
  (require 'nucleus-cli))
(after! package
  (require 'nucleus-packages))

(nucleus-initialize noninteractive)
(unless noninteractive
  (nucleus-initialize-modules))

(provide 'nucleus)
;;; nucleus.el ends here
