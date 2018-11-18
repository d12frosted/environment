;;; dna.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;; URL: https://github.com/d12frosted/environment/emacs
;;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(nucleus!
 :feature
 debugger
 eval
 (evil +everywhere)
 file-templates
 (lookup
  +docsets)
 snippets
 spellcheck
 (syntax-checker
  +childframe)
 workspaces

 :completion
 (company
  +auto)
 (ivy
  +fuzzy)

 :ui
 theme
 dashboard
 doom-quit
 evil-goggles
 ;;fci
 hl-todo
 modeline
 nav-flash
 treemacs
 (popup
  +all
  +defaults)
 vc-gutter
 vi-tilde-fringe
 window-select

 :editor
 (format +onsave)
 multiple-cursors
 ;;parinfer
 rotate-text

 :emacs
 dired
 ediff
 electric
 eshell
 hideshow
 imenu
 term
 vc

 :tools
 ;;docker
 editorconfig
 gist
 ;;macos
 make
 magit
 ;;password-store
 ;;pdf
 ;;rgb
 ;;terraform
 ;;tmux

 :lang
 data
 emacs-lisp
 (haskell +intero)
 latex
 ledger
 lua
 markdown
 nix
 (org
  +agenda
  +attach
  +babel
  +capture
  +export
  +present)
 plantuml
 rest
 ruby
 scala
 (sh +fish)

 ;; Applications are complex and opinionated modules that transform Emacs
 ;; toward a specific purpose. They may have additional dependencies and
 ;; should be loaded late.
 :app
 ;;(email +gmail)
 ;;irc
 ;;(rss +org)
 ;;twitter
 ;;(write
 ;; +wordnut
 ;; +langtool)

 :collab
 ;;floobits
 ;;impatient-mode

 :config
 (default +bindings +snippets +evil-commands))

(setq nucleus-theme 'leuven
      nucleus-font (font-spec :family "Source Code Pro" :size 12)
      nucleus-big-font (font-spec :family "Source Code Pro" :size 18)
      nucleus-serif-font (font-spec :family "Source Code Pro" :size 12))

(provide 'dna)

;;; dna.el ends here
