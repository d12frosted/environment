;;; config-path.el --- Path constants -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module defines path constants used across other modules.
;;
;;; Code:

(defconst path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a value of $HOME.")

(defconst path-cloud-dir (expand-file-name "Library/Mobile Documents/com~apple~CloudDocs/" path-home-dir)
  "Path to a cloud storage directory.")

;; (defconst path-cloud-dir (expand-file-name "Library/CloudStorage/Dropbox/" path-home-dir)
;;   "Path to a cloud storage directory.")

(defconst path-config-dir
  (file-name-as-directory
   (or (getenv "XDG_CONFIG_HOME")
       (concat path-home-dir ".config")))
  "The root directory for personal configurations.")

(defconst path-emacs-dir
  (file-name-as-directory
   (expand-file-name "emacs/" path-config-dir))
  "The path to this Emacs directory.")

(defconst path-autoloads-file
  (expand-file-name "lisp/init-autoloads.el" path-emacs-dir)
  "The path to personal autoloads file.")

(defconst path-emacs-private-dir
  (concat path-cloud-dir "emacs/")
  "The root directory for private configurations.")

(defconst path-local-dir
  (concat
   (file-name-as-directory
    (or (getenv "XDG_CACHE_HOME")
        (concat path-home-dir ".cache")))
   "emacs/")
  "The root directory for local Emacs files.

Use this as permanent storage for files that are safe to share
across systems.")

(defconst path-etc-dir (concat path-local-dir "etc/")
  "Directory for non-volatile storage.

Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defconst path-cache-dir (concat path-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defconst path-packages-dir
  (expand-file-name (format "packages/%s.%s/"
                            emacs-major-version
                            emacs-minor-version)
                    path-local-dir)
  "Where packages are stored.")

(defconst path-projects-dir
  (file-name-as-directory
   (or (getenv "PROJECTS_HOME")
       (concat path-home-dir "Developer")))
  "The root directory for projects.")

(provide 'config-path)
;;; config-path.el ends here
