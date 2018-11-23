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
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t

 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 2)

;; `helpful'
(define-key! 'global
  [remap describe-function] #'helpful-callable
  [remap describe-command]  #'helpful-command
  [remap describe-variable] #'helpful-variable
  [remap describe-key]      #'helpful-key)

(def-package! ws-butler
  :config
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
                '(special-mode comint-mode term-mode eshell-mode)))
  (ws-butler-global-mode))
