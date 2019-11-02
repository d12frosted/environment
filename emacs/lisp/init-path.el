;;; init-path.el --- path constant and utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;
;; Path
;;

(defconst +path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a value of $HOME.")

(defconst +path-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat +path-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst +path-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this Emacs directory.")

(defconst +path-emacs-private-dir
  (concat +path-home-dir "Dropbox/Apps/Emacs/")
  "The root directory for private configurations.")

(defconst +path-local-dir
  (concat
   (file-name-as-directory
    (or (getenv "XDG_CACHE_HOME")
        (concat +path-home-dir ".cache")))
   "emacs/")
  "The root directory for local Emacs files.

Use this as permanent storage for files that are safe to share
across systems.")

(defconst +path-etc-dir (concat +path-local-dir "etc/")
  "Directory for non-volatile storage.

Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defconst +path-cache-dir (concat +path-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defconst +path-packages-dir (concat +path-local-dir "packages/")
  "Where package.el and quelpa plugins are stored.")

(defconst +path-elpa-mirror-dir (concat +path-home-dir ".elpa-mirror/")
  "Path to (M)ELPA mirror.")

(provide 'init-path)
;;; init-path.el ends here
