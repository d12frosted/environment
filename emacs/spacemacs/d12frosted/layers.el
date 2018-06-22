;;; layers.el --- d12frosted layer layers file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(configuration-layer/declare-layers
 '(
   ;; utilities
   better-defaults
   ivy
   spacemacs-completion
   spacemacs-editing
   spacemacs-layouts
   spacemacs-project
   spacemacs-navigation
   spacemacs-visual
   version-control
   (git :variables
        git-magit-status-fullscreen t)
   github
   (auto-completion :disabled-for org spacemacs-org)
   spell-checking
   (syntax-checking :variables syntax-checking-enable-tooltips nil)
   dash

   ;; private layers
   d12-core
   d12-visual
   d12-text
   d12-org
   vulpea
   d12-emacs-lisp
   d12-haskell
   d12-csharp
   d12-applescript

   ;; languages
   scala
   shell-scripts
   yaml
   latex))

;;; layers.el ends here
