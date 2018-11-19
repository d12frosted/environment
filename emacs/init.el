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
;;   domain-...   public variables or non-interactive functions
;;   domain:...   private anything (non-interactive), not safe for direct use
;;   domain/...   an interactive function; safe for M-x or keybinding
;;   domain|...   hook function
;;   domain*...   advising functions
;;
;;; Code:

;; Introduce myself.
(setq user-full-name "Boris Buliga"
      user-mail-address "boris@d12frosted.io")

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
(require 'nucleus (concat user-emacs-directory "nucleus/nucleus"))

;;; init.el ends here
