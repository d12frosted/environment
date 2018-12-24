;;; lang/scala/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 12 Dec 2018
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

(after! scala-mode
  (setq scala-indent:align-parameters t))

(after! ensime
  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints 'all
        ensime-completion-style nil)

  (set-company-backend! 'scala-mode '(ensime-company company-yasnippet))

  ;; Fix void-variable imenu-auto-rescan error caused by `ensime--setup-imenu'
  ;; trying to make imenu variables buffer local before imenu is loaded.
  (require 'imenu))

(def-package! sbt-mode
  :after scala-mode
  :config (set-repl-handler! 'scala-mode #'run-scala))
