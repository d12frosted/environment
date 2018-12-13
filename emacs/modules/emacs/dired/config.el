;;; emacs/dired/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 13 Dec 2018
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

(def-package! dired
  :defer t
  :init
  ;; visit file or directory in place, reuse buffers
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
   ; Always copy/delete recursively
   dired-recursive-copies  'always
   dired-recursive-deletes 'top
   ;; Auto refresh dired, but be quiet about it
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil
   dired-hide-details-hide-symlink-targets nil))
