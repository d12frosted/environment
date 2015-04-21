;;; packages.el --- d12frosted Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-packages
  '(2048-game)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted/init-2048-game ()
  "Initialize 2048-game package."
  (use-package 2048-game
    :defer t
    :init
    :config))
