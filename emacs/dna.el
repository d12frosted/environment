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
 :feature
 ;; debugger
 ;; eval
 ;; (evil +everywhere)
 file-templates
 ;; (lookup
 ;;  +docsets)
 ;; snippets
 ;; spellcheck
 ;; (syntax-checker
 ;;  +childframe)
 ;; workspaces

 :completion
 (company
  +auto)
 ;; (ivy
 ;;  +fuzzy)

 :ui
 ;; theme
 ;; dashboard
 ;; doom-quit
 ;; evil-goggles
 ;; fci
 ;; hl-todo
 ;; modeline
 ;; nav-flash
 ;; treemacs
 ;; (popup
 ;;  +all
 ;;  +defaults)
 ;; vc-gutter
 ;; vi-tilde-fringe
 ;; window-select

 :editor
 ;; (format +onsave)
 ;; multiple-cursors
 ;; parinfer
 ;; rotate-text

 :emacs
 ;; dired
 ;; ediff
 ;; electric
 ;; eshell
 ;; hideshow
 ;; imenu
 ;; term
 ;; vc

 :tools
 ;; docker
 ;; editorconfig
 ;; gist
 ;; macos
 ;; make
 ;; magit
 ;; password-store
 ;; pdf
 ;; rgb
 ;; terraform
 ;; tmux

 :lang
 ;; data
 ;; emacs-lisp
 ;; (haskell +intero)
 ;; latex
 ;; ledger
 ;; lua
 ;; markdown
 ;; nix
 ;; (org-deprecated
 ;;  +agenda
 ;;  +attach
 ;;  +babel
 ;;  +capture
 ;;  +export
 ;;  +present)
 ;; plantuml
 ;; rest
 ;; ruby
 ;; scala
 ;; (sh +fish)

 :app
 ;; (email +gmail)
 ;; irc
 ;; (rss +org)
 ;; twitter
 ;; (write
 ;;  +wordnut
 ;;  +langtool)

 :collab
 ;; floobits
 ;; impatient-mode

 :config
 default)

(setq nucleus-theme 'leuven
      nucleus-font (font-spec :family "Source Code Pro" :size 12)
      nucleus-big-font (font-spec :family "Source Code Pro" :size 18)
      nucleus-serif-font (font-spec :family "Source Code Pro" :size 12))
