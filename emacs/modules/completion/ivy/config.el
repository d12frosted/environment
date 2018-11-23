;;; completion/ivy/config.el -*- lexical-binding: t; -*-
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

(def-package! ivy
  :defer 1
  :after-call pre-command-hook
  :config
  (setq projectile-completion-system 'ivy)
  (ivy-mode +1))

(def-package! counsel
  :commands counsel-describe-face
  :init
  (define-key! 'global
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap describe-face]            #'counsel-faces
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap imenu]                    #'counsel-imenu
    [remap recentf-open-files]       #'counsel-recentf
    [remap org-capture]              #'counsel-org-capture
    [remap swiper]                   #'counsel-grep-or-swiper))
