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

(require 'init-path)
(require 'init-package)
(require 'init-lsp)

(use-package scala-mode
  :hook
  (scala-mode . subword-mode)
  (scala-mode . lsp)
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (+scala-bootstrap)
  :config
  (setq scala-indent:align-parameters t
        ;; indent block comments to first asterix, not second
        scala-indent:use-javadoc-style t))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :after scala-mode
  :config
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(defun +scala-bootstrap ()
  "Bootstrap Scala development."
  (+scala--install-bin
   "coursier"
   (lambda (path)
     (shell-command
      (format "curl -L -o %s https://git.io/coursier-cli" path)
      "*scala-bootstrap*")
     (shell-command
      (format "chmod +x %s" path)
      "*scala-bootstrap*")))

  (+scala--install-bin
   "metals-emacs"
   (lambda (path)
     (shell-command
      (string-join
       (list "coursier bootstrap"
             "--java-opt -Xss4m"
             "--java-opt -Xms100m"
             "--java-opt -Dmetals.client=emacs"
             "org.scalameta:metals_2.12:0.9.0"
             "-r bintray:scalacenter/releases"
             "-r sonatype:snapshots"
             (format "-o %s -f" path))
       " ")
      "*scala-bootstrap*"))))

(defun +scala--install-bin (bin install-fn)
  "Install BIN if it's missing using INSTALL-FN."
  (unless (executable-find bin)
    (let ((path (concat +path-home-dir ".local/bin/" bin)))
      (funcall install-fn path))))

(provide 'init-scala)
;;; init-scala.el ends here
