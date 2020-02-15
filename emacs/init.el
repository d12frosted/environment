;;; init.el --- Emacs initialisation file -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Oct 2019
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(setq-default +package-archives 'upstream
              +benchmark-enable nil)

(defvar +gc-cons-threshold 16777216     ; 16mb
  "The default value to use for `gc-cons-threshold'.

If you experience freezing, decrease this. If you experience
stuttering, increase this.")

(defvar +gc-cons-upper-limit 268435456 ; 256mb
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar +gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar +file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold +gc-cons-upper-limit)
(add-hook
 'emacs-startup-hook
 (lambda ()
   "Restore default values after startup."
   (setq file-name-handler-alist +file-name-handler-alist)
   (setq gc-cons-threshold +gc-cons-threshold)

   ;; GC automatically while unfocusing the frame.
   (add-function :after
                after-focus-change-function
                #'garbage-collect)

   ;; Avoid GCs while using `ivy'.
   ;;
   ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
   (defun +minibuffer-setup-hook ()
     (setq gc-cons-threshold +gc-cons-upper-limit))

   (defun +minibuffer-exit-hook ()
     (setq gc-cons-threshold +gc-cons-threshold))

   (add-hook 'minibuffer-setup-hook #'+minibuffer-setup-hook)
   (add-hook 'minibuffer-exit-hook #'+minibuffer-exit-hook)))

;;
;; Load path
;;

(setq user-emacs-directory (file-name-directory (file-truename load-file-name)))
(defun +load-path--optimise (&rest _)
  "Optimise `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun +load-path--add-subdirs (&rest _)
  "Add lisp/** to `load-path'."
  (let ((default-directory
          (expand-file-name "lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'+load-path--optimise)
(advice-add #'package-initialize :after #'+load-path--add-subdirs)

(+load-path--optimise)

;; Defer `package-initialize'
;; Without this comment Emacs25 adds (package-initialize) here

;;
;; Configurations
;;

;; core
(require 'init-path)
(require 'init-package)
(require 'init-env)
(require 'init-base)
(require 'init-keybindings)

;; utitlies
(require 'init-buffer)
(require 'init-window)
(require 'init-ivy)
(require 'init-project)
(require 'init-completion)
(require 'init-navigation)
(require 'init-syntax-check)
(require 'init-spellcheck)
(require 'init-vcs)
(require 'init-dired)
(require 'init-pdf)
(require 'init-file-templates)
(require 'init-lsp)
(require 'init-test)

;; ui
(require 'init-ui)

;; languages
(require 'init-elisp)
(require 'init-haskell)
(require 'init-org)
(require 'init-sh)
(require 'init-rest)
(require 'init-puml)
(require 'init-proto)

;; other
(require 'init-fun)

(provide 'init)
;;; init.el ends here
