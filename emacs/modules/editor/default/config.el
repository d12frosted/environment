;;; editor/default/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
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

(setq-default
 vc-follow-symlinks t

 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t

 ;; Bookmarks
 bookmark-default-file (concat nucleus-etc-dir "bookmarks")
 bookmark-save-flag t

 ;; Formatting
 delete-trailing-lines nil ; `ws-butler' is used for better whitespace handling
 fill-column 80
 sentence-end-double-space nil
 word-wrap t

 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 2)

(def-package! autorevert
  ;; revert buffers for changed files
  :after-call after-find-file
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(def-package! ialign
  :defer t)
