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
  (setq scala-indent:align-parameters t
        ;; indent block comments to first asterix, not second
        scala-indent:use-javadoc-style t)

  (setq-hook! 'scala-mode-hook comment-line-break-function #'+scala-comment-indent-new-line)

  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(scala-mode c/c++/java scala-indent:step)))

  (when (featurep! +lsp)
    (add-hook 'scala-mode-hook #'lsp)))

(def-package! sbt-mode
  :after scala-mode
  :config (set-repl-handler! 'scala-mode #'run-scala))
