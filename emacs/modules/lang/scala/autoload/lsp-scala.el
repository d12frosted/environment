;;; lang/scala/autoload/lsp-scala.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Ross A. Baker <ross@rossabaker.com>
;;         Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 27 Dec 2018
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

(require 'lsp-mode)
(require 'sbt-mode)

;;;###autoload
(defcustom lsp-scala-server-command "metals-emacs"
  "The command to launch the Scala language server."
  :group 'lsp-scala
  :type 'file)

;;;###autoload
(defcustom lsp-scala-server-args '()
  "Extra arguments for the Scala language server."
  :group 'lsp-scala
  :type '(repeat string))

(defun lsp-scala--server-command ()
  "Generate the Scala language server startup command."
  `(,lsp-scala-server-command ,@lsp-scala-server-args))

(defvar lsp-scala--config-options `())

(defun lsp-scala--set-configuration ()
  "Set the configuration for the Scala LSP server."
  (lsp--set-configuration `(:metals ,lsp-scala--config-options)))

(add-hook 'lsp-after-initialize-hook 'lsp-scala--set-configuration)

(defun lsp-scala-build-import ()
  "Unconditionally run `sbt bloopInstall` and re-connect to the build server."
  (interactive)
  (lsp-send-execute-command "build-import" ()))

(defun lsp-scala-build-connect ()
  "Unconditionally cancel existing build server connection and re-connect."
  (interactive)
  (lsp-send-execute-command "build-connect" ()))

(defun lsp-scala-doctor-run ()
  "Open the Metals doctor to troubleshoot potential build problems."
  (interactive)
  (lsp-send-execute-command "doctor-run" ()))

(defun lsp-scala-sources-scan ()
  "Walk all files in the workspace and index where symbols are defined."
  (interactive)
  (lsp-send-execute-command "source-scan" ()))

;;;###autoload
(defun lsp-scala-setup ()
  "Setup lsp for Scala development."
  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection 'lsp-scala--server-command)
		                :major-modes '(scala-mode)
		                :server-id 'scala)))
