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

(defconst known-bases '("spacemacs" "doom" "vulpea")
  "List of the known bases to load from.")

(defvar basis (or (car (member (getenv "EMACS_BASIS") known-bases))
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
(when (string-equal basis "spacemacs")
  (message "Loading Spacemacs: " path-spacemacs-distr-home)
  (setq-default
   spacemacs-start-directory path-spacemacs-distr-home
   dotspacemacs-filepath (if debug-mode
                             path-spacemacs-user-config-test-file
                           path-spacemacs-user-config-file))
  (load-file path-spacemacs-distr-init-file))

;; load doom
(when (string-equal basis "doom")
  (message "Loading doom: %s" path-doom-distr-home)
  (load-file (concat path-doom-distr-home "init.el")))

;; The worst key binding ever! If I ever want to quit Emacs, I'd call my doctor.
(define-key global-map (kbd "C-x C-c") nil)

;; I use meta a lot, and command key is much easier to hit than option.
(setq mac-command-modifier 'meta
      mac-option-modifier  'none)

;;; init.el ends here
