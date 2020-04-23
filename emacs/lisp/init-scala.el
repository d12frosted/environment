;;; init-scala.el --- Scala support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Apr 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-package)
(require 'init-lsp)

(use-package scala-mode
  :hook
  (scala-mode . subword-mode)
  (scala-mode . lsp)
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (setq scala-indent:align-parameters t
        ;; indent block comments to first asterix, not second
        scala-indent:use-javadoc-style t))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :after scala-mode
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(provide 'init-scala)
;;; init-scala.el ends here
