;;; init.el --- init file for personal Spacemacs configurations -*- lexical-binding: t; -*-
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
;;; Code:

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))

(defconst path-home
  (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a Value of $HOME.")

(defconst user-home-directory
  path-home
  "Alias for `path-home'.")

(defconst path-config-home
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat path-home ".config")))
  "Path to user-wide configurations directory.

Defaults to $XDG_CONFIG_HOME when the variable is present and
non-nil.")

(defconst path-cache-home
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_CACHE")
       (concat path-home ".cache")))
  "Path to user-wide cache  directory.

Defaults to $XDG_CONFIG_CACHE when the variable is present and
non-nil.")

(defconst path-dropbox
  (concat path-home "Dropbox/")
  "Path to Dropbox directory.")

(defconst path-org-home
  (concat path-dropbox "vulpea/")
  "Path to Org directory.")

(defconst path-org-notes-home
  (concat path-org-home "notes/")
  "Path to Org Notes directory.")

(defconst path-org-tasks-home
  (concat path-org-home "gtd/")
  "Path to Org Tasks directory.")

(defconst path-emacs-home
  (concat path-config-home "emacs/")
  "Path to Emacs configurations directory.")

(defconst path-emacs-cache
  (concat path-cache-home "emacs/")
  "Path to Emacs cache directory.")

(defconst path-emacs-private
  (concat path-dropbox "Apps/Emacs/")
  "Path to Emacs private configurations directory.")

(defconst path-emacs-local
  (file-name-as-directory
   (or (getenv "XDG_DATA_HOME")
       (concat path-home ".local/share")))
  "Path to Emacs local configurations directory.

Defaults to $XDG_DATA_HOME when the variable is present and
non-nil.")

(defconst path-spacemacs-config-home
  (concat path-config-home "spacemacs/")
  "Path to Spacemacs configurations directory.")

(defconst path-spacemacs-user-config-file
  (concat path-spacemacs-config-home "spacemacs.el")
  "Path to Spacemacs user configuration file.")

(defconst path-spacemacs-user-config-test-file
  (concat path-spacemacs-config-home "spacemacs-test.el")
  "Path to Spacemacs user configuration test file.")

(defconst path-spacemacs-distr-home
  (concat path-home ".spacemacs/")
  "Path to Spacemacs distribution directory.")

(defconst path-spacemacs-distr-init-file
  (concat path-spacemacs-distr-home "init.el")
  "Path to Spacemacs distribution init file.")

(defconst path-doom-distr-home
  (concat path-home ".doom-emacs/")
  "Path to doom distribution directory.")

(defconst path-fish-config-home
  (concat path-config-home "fish/")
  "Path to fish configurations directory.")

(defconst path-fish-private-config-home
  (concat path-dropbox "Apps/fish/")
  "Path to private fish configurations directory.")

(defconst path-projects-home
  (concat path-home "Developer/")
  "Path to projects directory.")

(defconst path-custom-file
  (concat path-emacs-private "custom.el")
  "Path to 'custom.el' file.")

(defconst path-elpa-mirror-home
  (concat path-projects-home "elpa-mirror/")
  "Path to ELPA mirror directory.")

(defun path-get-org-file (file)
  "Locate org FILE relative to `path-org-home'."
  (concat path-org-home file ".org"))

(defun path-get-org-note-file (file)
  "Locate org FILE relative to `path-org-home'."
  (concat path-org-notes-home file ".org"))

(defun path-get-org-task-file (file)
  "Locate org FILE relative to `path-org-home'."
  (concat path-org-tasks-home file ".org"))

(defun path-get-org-dir (dir)
  "Locate org DIR relative to `path-org-home'."
  (file-name-as-directory
   (concat path-org-home dir)))

(defun path-make-directory-safe (path &optional parents)
  "Create the directory PATH and optionally any nonexistent PARENTS.
If PATH already exists as a directory, just do nothing."
  (unless (file-directory-p path)
    (make-directory path parents)))

(defun path-load-project (name)
  "Add project NAME from `path-projects-home' to `load-path'."
  (add-to-load-path-if-exists (concat path-projects-home name)))

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

(setq user-emacs-directory (file-name-directory (file-truename load-file-name)))

(setq-default
 spacemacs-start-directory path-spacemacs-distr-home
 dotspacemacs-filepath (if (getenv "DEBUG_EMACS")
                           path-spacemacs-user-config-test-file
                         path-spacemacs-user-config-file))
(load-file path-spacemacs-distr-init-file)

;; The worst key binding ever! If I ever want to quit Emacs, I'd call my doctor.
(define-key global-map (kbd "C-x C-c") nil)

;; I use meta a lot, and command key is much easier to hit than option.
(setq mac-command-modifier 'meta
      mac-option-modifier  'none)

(provide 'init)

;;; init.el ends here
