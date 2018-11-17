;;; nucleus.el --- the heart of every cell -*- lexical-binding: t; -*-
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

(eval-when-compile
  (and (version< emacs-version "26")
       (error "Detected Emacs %s. Only Emacs 26.1 and higher is supported."
              emacs-version)))

(defvar nucleus-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all nucleus functions will be verbose.

Set DEBUG=1 in the command line or use --debug-init to enable
this.")

;;
;; Constants

(defconst nucleus-projects-dir (concat user-home-directory "Developer/")
  "Directory for personal projects.")

(defconst nucleus-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs directory. Must end in a slash.")

(defconst nucleus-dir (concat nucleus-emacs-dir "nucleus/")
  "The root directory of Nucleus files.")

(defconst nucleus-modules-dir (concat nucleus-emacs-dir "modules/")
  "The root directory for modules.")

;; TODO: move outside of $XDG_CONFIG_HOME
(defconst nucleus-local-dir (concat nucleus-emacs-dir ".local/")
  "The root directory for local Emacs files.

Use this as permanent storage for files that are safe to share
across systems.")

(defconst nucleus-etc-dir (concat nucleus-local-dir "etc/")
  "Directory for non-volatile storage.

Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defconst nucleus-cache-dir (concat nucleus-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defconst nucleus-packages-dir (concat nucleus-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.")

(defconst nucleus-autoload-file (concat nucleus-local-dir "autoloads.el")
  "Where `nucleus-reload-nucleus-autoloads' will generate its
  core autoloads file.")

(defconst nucleus-package-autoload-file (concat nucleus-local-dir "autoloads.pkg.el")
  "Where `nucleus-reload-package-autoloads' will generate its
package.el autoloads file.")

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

;;
;; Variables

(defvar nucleus-assembled-p nil
  "Non-nil if `nucleus-assemble' has run.")

(defvar nucleus-init-time nil
  "The time it took, in seconds, for Emacs to initialize.")

(defvar nucleus--refreshed-p nil)

;;
;; Custom error types

(define-error 'nucleus-error "Error in the nucleus")
(define-error 'nucleus-hook-error "Error in a startup hook" 'nucleus-error)
(define-error 'nucleus-autoload-error "Error in an autoloads file" 'nucleus-error)
(define-error 'nucleus-module-error "Error in a module" 'nucleus-error)
(define-error 'nucleus-package-error "Error with packages" 'nucleus-error)

;;
;; Custom hooks

;;
;; Emacs nucleus configurations

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq-default
 ;; update ui less often
 idle-update-delay 2

 ;; be quiet at startup
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil

 ;; history & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil                  ; don't create backup~ files

 ;; debug
 debug-on-error nucleus-debug-mode

 ;; byte compilation
 byte-compile-verbose nucleus-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)

 ;; security
 gnutls-verify-error (not (getenv "INSECURE"))
 tls-checktrust gnutls-verify-error
 tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                   ;; compatibility fallbacks
                   "gnutls-cli -p %p %h"
                   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

 ;; files
 abbrev-file-name (concat nucleus-local-dir "abbrev.el")
 auto-save-list-file-name (concat nucleus-cache-dir "autosave")
 backup-directory-alist (list (cons "." (concat nucleus-cache-dir "backup/")))
 mc/list-file (concat nucleus-etc-dir "mc-lists.el")
 pcache-directory (concat nucleus-cache-dir "pcache/")
 request-storage-directory (concat nucleus-cache-dir "request")
 server-auth-dir (concat nucleus-cache-dir "server/")
 shared-game-score-directory (concat nucleus-etc-dir "shared-game-score/")
 tramp-auto-save-directory (concat nucleus-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name (concat nucleus-cache-dir "tramp-persistency.el")
 url-cache-directory (concat nucleus-cache-dir "url/")
 url-configuration-directory (concat nucleus-etc-dir "url/")
 gamegrid-user-score-file-directory (concat nucleus-etc-dir "games/")
 )

;;
;; Assemble helpers

(defun nucleus-ensure-directories-exist ()
  "Make sure all essential local directories exist."
  (dolist (dir (list nucleus-local-dir
                     nucleus-etc-dir
                     nucleus-cache-dir
                     nucleus-packages-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun nucleus-assemble (&optional force-p force-load-nucleus-p)
  "Assemble the nucleus if it's not assembled yet.

Nucleus will be assembled iff FORCE-P is is t or
`nucleus-assembled-p' is nil.

The assemble process involves the following steps:

1. create essential directories and files
2. install all nucleus packages
3. load all nucleus files"
  (when (or force-p (not nucleus-assembled-p))
    (setq nucleus-assembled-p t)

    ;; `nucleus-autoload-file' tells Emacs where to load all its autoloaded
    ;; functions from. This includes everything in nucleus/autoload/*.el and all
    ;; the autoload files in your enabled modules.
    (when (or force-p (not (nucleus-assemble-autoloads nucleus-autoload-file)))
      (nucleus-ensure-directories-exist)

      (require 'nucleus-packages)
      (nucleus-ensure-packages-initialised force-p)
      (nucleus-ensure-core-packages)

      (unless (or force-p noninteractive)
        (user-error "Your nucleus autoloads are missing! Run `bin/nucleus refresh' to regenerate them")))

    ;; Loads `nucleus-package-autoload-file', which loads a concatenated package
    ;; autoloads file and caches `load-path', `auto-mode-alist',
    ;; `Info-directory-list', `nucleus-disabled-packages' and
    ;; `package-activated-list'. A big reduction in startup time.
    (unless (or force-p
                (nucleus-assemble-autoloads nucleus-package-autoload-file)
                noninteractive)
      (user-error "Your package autoloads are missing! Run `bin/nucleus refresh' to regenerate them")))

  (require 'nucleus-os)
  (when (or force-load-nucleus-p (not noninteractive))
    (add-hook! 'emacs-startup-hook
      #'(nucleus|init-switch-hooks nucleus|display-benchmark))

    (require 'nucleus-ui)
    (require 'nucleus-editor)
    (require 'nucleus-projects)
    (require 'nucleus-keybinds)))

(defun nucleus-assemble-autoloads (file)
  "Assemble an autoload FILE.

Possible result:

- t on success
- exception on failure in interactive session
- nil otherwise"
  (condition-case e
      (load (file-name-sans-extension file) 'noerror 'nomessage)
    ((debug error)
     (if noninteractive
         (message "Autoload file warning: %s -> %s" (car e) (error-message-string e))
       (signal 'nucleus-autoload-error (list (file-name-nondirectory file) e))))))

;;
;; Assemble

(add-to-list 'load-path nucleus-dir)

(require 'nucleus-lib)
(require 'nucleus-modules)

(when noninteractive
  (require 'nucleus-cli))
(after! package
  (require 'nucleus-packages))

(nucleus-assemble noninteractive)
(unless noninteractive
  (nucleus-assemble-modules))

(provide 'nucleus)

;;; nucleus.el ends here
