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
;;   bb-...   public variables or non-interactive functions
;;   bb:...   private anything (non-interactive), not safe for direct use
;;   bb/...   an interactive function; safe for M-x or keybinding
;;   bb|...   hook function
;;   bb*...   advising functions
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

(defvar use-spacemacs (getenv "EMACS_SPACEMACS")
  "Automatically load Spacemacs.")

(defvar use-doom (getenv "EMACS_DOOM")
  "Automatically load doom.")

;; normalize distribution
(when use-spacemacs
  (setq use-doom nil))
(when use-doom
  (setq use-spacemacs nil))

;; setup emacs configuration
(setq user-init-file (file-truename (or load-file-name (buffer-file-name))))
(setq user-emacs-directory (file-name-directory user-init-file))

;; load some core features
(require 'bb-path (concat user-emacs-directory "core/bb-path"))

;; setup and load `custom-file'
(setq custom-file bb-path-custom-file)
(load custom-file t)

;; load `private.el' file containing all the sensitive data
(let ((private-file (concat bb-path-emacs-private "private.el")))
  (when (file-exists-p private-file)
    (load private-file)))

;; load `local.el' file containing all the machine specific configurations
(let ((local-file (concat bb-path-emacs-local "local.el")))
  (when (file-exists-p local-file)
    (load local-file)))

;; setup package-user-dir to allow seamless switch between emacs versions
(setq package-user-dir
      (file-name-as-directory
       (concat bb-path-emacs-cache "elpa/" emacs-version)))

;; load spacemacs
(when use-spacemacs
  (message "Loading Spacemacs: " bb-path-spacemacs-distr-home)
  (setq-default
   spacemacs-start-directory bb-path-spacemacs-distr-home
   dotspacemacs-filepath (if debug-mode
                             bb-path-spacemacs-user-config-test-file
                           bb-path-spacemacs-user-config-file))
  (load-file bb-path-spacemacs-distr-init-file))

;; load doom
(when use-doom
  (message "Loading doom: %s" bb-path-doom-distr-home)
  (load-file (concat bb-path-doom-distr-home "init.el")))

;; The worst key binding ever! If I ever want to quit Emacs, I'd call my doctor.
(define-key global-map (kbd "C-x C-c") nil)

;; I use meta a lot, and command key is much easier to hit than option.
(setq mac-command-modifier 'meta
      mac-option-modifier  'none)

;;; init.el ends here
