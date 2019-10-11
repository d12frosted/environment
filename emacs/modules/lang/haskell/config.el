;;; lang/haskell/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Dec 2018
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

(when (featurep! +intero)
  (load! "+intero"))

(when (featurep! +lsp)
  (load! "+lsp"))

(after! haskell-mode
  (setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
        haskell-process-auto-import-loaded-modules t)
  (when (featurep! :tools syntax-checker)
    (setq haskell-process-show-overlays nil))  ; flycheck makes this unnecessary
  (setq haskell-stylish-on-save t)
  (add-hook! 'haskell-mode-hook
    #'(subword-mode           ; improves text navigation with camelCase
       haskell-collapse-mode  ; support folding haskell code blocks
       interactive-haskell-mode))
  (set-file-template! 'haskell-mode :trigger #'haskell-auto-insert-module-template :project t)
  (set-repl-handler! '(haskell-mode haskell-cabal-mode literate-haskell-mode) #'+haskell-repl)

  (add-to-list 'completion-ignored-extensions ".hi"))
