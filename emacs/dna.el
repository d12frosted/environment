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
 file-templates
 projects
 snippets
 syntax-checker

 :completion
 (company
  +auto)
 ivy

 :ui

 :editor

 :emacs
 files
 ui

 :tools
 magit

 :lang
 emacs-lisp
 org
 rest

 :app

 :collab

 :config
 os)

(setq nucleus-theme 'leuven
      nucleus-font (font-spec :family "Source Code Pro" :size 12)
      nucleus-big-font (font-spec :family "Source Code Pro" :size 18)
      nucleus-serif-font (font-spec :family "Source Code Pro" :size 12))
