;;; init.el --- init file of d12frosted configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Commentary:

;; Private emacs configurations of Boris Buliga.
;;
;; User key prefixes:
;;
;; - C-c a: applications
;; - C-c c: current mode commands
;; - C-c o: Org mode
;; - C-c t: Toggle things and skeletons
;; - C-c u: Miscellaneous utilities
;; - C-c v: Version control

;;; Code

(require 'cl)

;;; About myself
;; ==============

(setq user-full-name "Michael Fogleman"
      user-mail-address "michaelwfogleman@gmail.com"
      user-github-url "https://github.com/d12frosted"
      user-home-url "http://d12frosted.github.io")

;;; Paths
;; =======

(defconst d12/cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Storage area for persistent files.")

(defconst d12/configs-directory
  (expand-file-name (concat user-emacs-directory "configs/"))
  "Configs directory.")

(defconst d12/packages-directory
  (expand-file-name (concat user-emacs-directory "packages/"))
  "Manually installed packages directory.")

(defconst pcache-directory
  (concat d12/cache-directory "pcache"))

;; create d12/cache-directory if it doesn't exist yet
(unless (file-exists-p d12/cache-directory)
    (make-directory d12/cache-directory))

;;; Configurations
;; ================

(defun d12/load-config (name)
  "Load configuration with NAME.

  Configuration is a set of two files:
  - NAME-funcs.el
  - NAME-configs.el

  that are located in 'd12/configs-directory."
  (load (concat d12/configs-directory name "-funcs.el"))
  (load (concat d12/configs-directory name "-configs.el")))

;;; Custom configurations
;; -----------------------

(defvar d12/configs '()
  "List of custom config names to load.")

(setq d12/custom-configs
      '("org"
        "magit"
        "syntax-checking"))

;; load core configurations
(d12/load-config "core")
