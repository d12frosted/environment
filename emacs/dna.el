;;; dna.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

(nucleus!
  :completion
 (company
  +auto)
 ivy

 :ui
 helpful
 modeline
 modern
 theming
 vc-gutter

 :editor
 default
 navigation
 octocursors
 whitespaces

 :emacs
 align
 buffer
 describe
 dired
 ediff
 eshell
 file
 string
 window

 :tools
 dir-settings
 eval
 file-templates
 projects
 snippets
 spellcheck
 syntax-checker
 workspaces
 (git +forge)
 lsp
 pdf

 :lang
 dot
 emacs-lisp
 go
 (haskell +intero)
 nix
 org
 plantuml
 proto
 rest
 (scala +lsp)
 sh

 :app

 :collab

 :config
 keybindings
 os
 private
 )
