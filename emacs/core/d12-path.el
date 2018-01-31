;;; d12-path.el --- d12-path file for personal configurations -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2017 Boris Buliga
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
;;   d12-path-*-file  path representing file
;;   d12-path-*       path representing directory
;;
;;; Code:

(defconst d12-path-home
  (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a Value of $HOME.")

(defconst user-home-directory
  d12-path-home
  "Alias for `d12-path-home'.")

(defconst d12-path-config-home
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat d12-path-home ".config")))
  "Path to user-wide configurations directory.

Defaults to $XDG_CONFIG_HOME when the variable is present and
non-nil.")

(defconst d12-path-cache-home
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_CACHE")
       (concat d12-path-home ".cache")))
  "Path to user-wide cache  directory.

Defaults to $XDG_CONFIG_CACHE when the variable is present and
non-nil.")

(defconst d12-path-dropbox
  (concat d12-path-home "Dropbox/")
  "Path to Dropbox directory.")

(defconst d12-path-org-home
  (concat d12-path-dropbox "org/")
  "Path to Org directory.")

(defconst d12-path-emacs-home
  (concat d12-path-config-home "emacs/")
  "Path to Emacs configurations directory.")

(defconst d12-path-emacs-cache
  (concat d12-path-cache-home "emacs/")
  "Path to Emacs cache directory.")

(defconst d12-path-emacs-private
  (concat d12-path-dropbox "Apps/Emacs/")
  "Path to Emacs private configurations directory.")

(defconst d12-path-spacemacs-config-home
  (concat d12-path-emacs-home "spacemacs/")
  "Path to Spacemacs configurations directory.")

(defconst d12-path-spacemacs-user-config-file
  (concat d12-path-spacemacs-config-home "spacemacs.el")
  "Path to Spacemacs user configuration file.")

(defconst d12-path-spacemacs-user-config-test-file
  (concat d12-path-spacemacs-config-home "spacemacs-test.el")
  "Path to Spacemacs user configuration test file.")

(defconst d12-path-spacemacs-distr-home
  (concat d12-path-home ".spacemacs/")
  "Path to Spacemacs distribution directory.")

(defconst d12-path-spacemacs-distr-init-file
  (concat d12-path-spacemacs-distr-home "init.el")
  "Path to Spacemacs distribution init file.")

(defconst d12-path-fish-config-home
  (concat d12-path-config-home "fish/")
  "Path to fish configurations directory.")

(defconst d12-path-fish-private-config-home
  (concat d12-path-dropbox "Apps/fish/")
  "Path to private fish configurations directory.")

(defconst d12-path-projects-home
  (concat d12-path-home "Developer/")
  "Path to projects directory.")

(defconst d12-path-custom-file
  (concat d12-path-emacs-private "custom.el")
  "Path to 'custom.el' file.")

(defconst d12-path-elpa-mirror-home
  (concat d12-path-projects-home "elpa-mirror/")
  "Path to ELPA mirror directory.")

(defun d12-path/get-org-file (file)
  "Locate org FILE relative to `d12-path-org-home'."
  (concat d12-path-org-home file ".org"))

(defun d12-path/get-org-dir (dir)
  "Locate org DIR relative to `d12-path-org-home'."
  (file-name-as-directory
   (concat d12-path-org-home dir)))

(provide 'd12-path)
;;; d12-path.el ends here
