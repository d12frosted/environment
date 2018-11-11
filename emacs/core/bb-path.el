;;; bb-path.el --- bb-path file for personal configurations -*- lexical-binding: t; -*-
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
;; Path constants and helpers.
;;
;; Naming conventions:
;;   bb-path-*-file  path representing file
;;   bb-path-*       path representing directory
;;
;;; Code:

(defconst bb-path-home
  (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a Value of $HOME.")

(defconst user-home-directory
  bb-path-home
  "Alias for `bb-path-home'.")

(defconst bb-path-config-home
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat bb-path-home ".config")))
  "Path to user-wide configurations directory.

Defaults to $XDG_CONFIG_HOME when the variable is present and
non-nil.")

(defconst bb-path-cache-home
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_CACHE")
       (concat bb-path-home ".cache")))
  "Path to user-wide cache  directory.

Defaults to $XDG_CONFIG_CACHE when the variable is present and
non-nil.")

(defconst bb-path-dropbox
  (concat bb-path-home "Dropbox/")
  "Path to Dropbox directory.")

(defconst bb-path-org-home
  (concat bb-path-dropbox "vulpea/")
  "Path to Org directory.")

(defconst bb-path-org-notes-home
  (concat bb-path-org-home "notes/")
  "Path to Org Notes directory.")

(defconst bb-path-org-tasks-home
  (concat bb-path-org-home "gtd/")
  "Path to Org Tasks directory.")

(defconst bb-path-emacs-home
  (concat bb-path-config-home "emacs/")
  "Path to Emacs configurations directory.")

(defconst bb-path-emacs-cache
  (concat bb-path-cache-home "emacs/")
  "Path to Emacs cache directory.")

(defconst bb-path-emacs-private
  (concat bb-path-dropbox "Apps/Emacs/")
  "Path to Emacs private configurations directory.")

(defconst bb-path-emacs-local
  (file-name-as-directory
   (or (getenv "XDG_DATA_HOME")
       (concat bb-path-home ".local/share")))
  "Path to Emacs local configurations directory.

Defaults to $XDG_DATA_HOME when the variable is present and
non-nil.")

(defconst bb-path-spacemacs-config-home
  (concat bb-path-emacs-home "spacemacs/")
  "Path to Spacemacs configurations directory.")

(defconst bb-path-spacemacs-user-config-file
  (concat bb-path-spacemacs-config-home "spacemacs.el")
  "Path to Spacemacs user configuration file.")

(defconst bb-path-spacemacs-user-config-test-file
  (concat bb-path-spacemacs-config-home "spacemacs-test.el")
  "Path to Spacemacs user configuration test file.")

(defconst bb-path-spacemacs-distr-home
  (concat bb-path-home ".spacemacs/")
  "Path to Spacemacs distribution directory.")

(defconst bb-path-spacemacs-distr-init-file
  (concat bb-path-spacemacs-distr-home "init.el")
  "Path to Spacemacs distribution init file.")

(defconst bb-path-doom-distr-home
  (concat bb-path-home ".doom-emacs/")
  "Path to doom distribution directory.")

(defconst bb-path-fish-config-home
  (concat bb-path-config-home "fish/")
  "Path to fish configurations directory.")

(defconst bb-path-fish-private-config-home
  (concat bb-path-dropbox "Apps/fish/")
  "Path to private fish configurations directory.")

(defconst bb-path-projects-home
  (concat bb-path-home "Developer/")
  "Path to projects directory.")

(defconst bb-path-custom-file
  (concat bb-path-emacs-private "custom.el")
  "Path to 'custom.el' file.")

(defconst bb-path-elpa-mirror-home
  (concat bb-path-projects-home "elpa-mirror/")
  "Path to ELPA mirror directory.")

(defun bb-path/get-org-file (file)
  "Locate org FILE relative to `bb-path-org-home'."
  (concat bb-path-org-home file ".org"))

(defun bb-path/get-org-note-file (file)
  "Locate org FILE relative to `bb-path-org-home'."
  (concat bb-path-org-notes-home file ".org"))

(defun bb-path/get-org-task-file (file)
  "Locate org FILE relative to `bb-path-org-home'."
  (concat bb-path-org-tasks-home file ".org"))

(defun bb-path/get-org-dir (dir)
  "Locate org DIR relative to `bb-path-org-home'."
  (file-name-as-directory
   (concat bb-path-org-home dir)))

(defun bb-path/make-directory-safe (path &optional parents)
  "Create the directory PATH and optionally any nonexistent PARENTS.
If PATH already exists as a directory, just do nothing."
  (unless (file-directory-p path)
    (make-directory path parents)))

(defun bb-path/load-project (name)
  "Add project NAME from `bb-path-projects-home' to `load-path'."
  (add-to-load-path-if-exists (concat bb-path-projects-home name)))

(provide 'bb-path)

;;; bb-path.el ends here
