;;; init.el --- init file for personal configurations -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; Naming conventions:
;;
;;   vulpea-...   public variables or non-interactive functions
;;   vulpea:...   private anything (non-interactive), not safe for direct use
;;   vulpea/...   an interactive function; safe for M-x or keybinding
;;   vulpea|...   hook function
;;   vulpea*...   advising functions
;;
;;; Code:

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))


;; disable package initialisation
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;;
;; Variables

(defvar debug-mode (getenv "DEBUG_EMACS")
  "Non nil enables debug mode. Whatever that means.")

(defconst known-bases '("nucleus" "doom" "spacemacs")
  "List of the known bases to load from.")

(defvar emacs-basis (or (car (member (getenv "EMACS_BASIS") known-bases))
                        (car known-bases))
  "Emacs basis to load from.")

;; ensure we are running out of this file's directory
(setq user-emacs-directory (file-name-directory (file-truename load-file-name)))

;; load path.el
(require 'path (concat user-emacs-directory "core/path"))

;; setup and load `custom-file'
(setq custom-file path-custom-file)
(load custom-file t)

;; load `private.el' file containing all the sensitive data
(let ((private-file (concat path-emacs-private "private.el")))
  (when (file-exists-p private-file)
    (load private-file)))

;; load `local.el' file containing all the machine specific configurations
(let ((local-file (concat path-emacs-local "local.el")))
  (when (file-exists-p local-file)
    (load local-file)))

;; setup package-user-dir to allow seamless switch between emacs versions
(setq package-user-dir
      (file-name-as-directory
       (concat path-emacs-cache "elpa/" emacs-version)))

;; load spacemacs
(when (string-equal emacs-basis "spacemacs")
  (message "Loading Spacemacs: " path-spacemacs-distr-home)
  (setq-default
   spacemacs-start-directory path-spacemacs-distr-home
   dotspacemacs-filepath (if debug-mode
                             path-spacemacs-user-config-test-file
                           path-spacemacs-user-config-file))
  (load-file path-spacemacs-distr-init-file)

  ;; The worst key binding ever! If I ever want to quit Emacs, I'd call my doctor.
  (define-key global-map (kbd "C-x C-c") nil)

  ;; I use meta a lot, and command key is much easier to hit than option.
  (setq mac-command-modifier 'meta
        mac-option-modifier  'none))

;; load doom
(when (string-equal emacs-basis "doom")
  (message "Loading doom: %s" path-doom-distr-home)
  (load-file (concat path-doom-distr-home "init.el")))

;; load nucleus
(when (string-equal emacs-basis "nucleus")
  (defvar nucleus-gc-cons-threshold 16777216 ; 16mb
    "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

  (defvar nucleus-gc-cons-upper-limit 268435456 ; 256mb
    "The temporary value for `gc-cons-threshold' to defer it.")

  (defvar nucleus:file-name-handler-alist file-name-handler-alist)

  (defun nucleus|restore-startup-optimizations ()
    "Resets garbage collection settings to reasonable defaults (a large
`gc-cons-threshold' can cause random freezes otherwise) and resets
`file-name-handler-alist'."
    (setq file-name-handler-alist nucleus:file-name-handler-alist)
    ;; Do this on idle timer to defer a possible GC pause that could result; also
    ;; allows deferred packages to take advantage of these optimizations.
    (run-with-idle-timer
     3 nil (lambda () (setq-default gc-cons-threshold nucleus-gc-cons-threshold))))

  (if (or after-init-time noninteractive)
      (setq gc-cons-threshold nucleus-gc-cons-threshold)
    ;; A big contributor to startup times is garbage collection. We up the gc
    ;; threshold to temporarily prevent it from running, then reset it later in
    ;; `nucleus|restore-startup-optimizations'.
    (setq gc-cons-threshold nucleus-gc-cons-upper-limit)
    ;; This is consulted on every `require', `load' and various path/io
    ;; functions. You get a minor speed up by nooping this.
    (setq file-name-handler-alist nil)
    ;; Not restoring these to their defaults will cause stuttering/freezes.
    (add-hook 'emacs-startup-hook #'nucleus|restore-startup-optimizations))

  ;; Ensure we are in the right directory.
  (setq user-emacs-directory (file-name-directory (file-truename load-file-name)))
  ;; In noninteractive sessions, prioritize non-byte-compiled source files to
  ;; prevent stale, byte-compiled code from running. However, if you're getting
  ;; recursive load errors, it may help to set this to nil.
  (setq load-prefer-newer noninteractive)

  ;; Assemble!
  (require 'nucleus (concat user-emacs-directory "nucleus/nucleus")))

;;; init.el ends here
