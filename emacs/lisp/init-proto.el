;;; init-proto.el --- Protobuf support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 14 Feb 2020
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

(require 'init-file)

(defvar-local +proto-custom-include-path nil
  "Path to custom library for protobuf checker.")

(use-package protobuf-mode
  :defer t
  :after flycheck
  :init
  (add-hook 'protobuf-mode-hook #'+proto|install-dependencies)
  :config
  (require 'flycheck)
  (flycheck-define-checker protobuf-protoc
    "A protobuf syntax checker using the protoc compiler.

See URL `https://developers.google.com/protocol-buffers/'."
    :command
    ("protoc" "--error_format" "gcc"
     (eval (concat "--java_out=" (flycheck-temp-dir-system)))
     ;; Add the file directory of protobuf path to resolve import
     ;; directives
     (eval (concat "--proto_path=" (+proto-working-directory)))
     "-I/usr/local/include"
     (eval
      (concat
       "-I"
       (getenv "GOPATH")
       "/src/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis"))
     (eval
      (when +proto-custom-include-path
        (concat
         "-I"
         +proto-custom-include-path)))
     source-inplace)
    :error-filter flycheck-fill-empty-line-numbers
    :error-patterns
    ((info line-start (file-name) ":" line ":" column ": note: " (message) line-end)
     (error line-start (file-name) ":" line ":" column ": " (message) line-end)
     (error line-start (file-name) ": " (message) line-end)
     (error line-start (message "In file included from") " " (file-name) ":" line ":" column ":" line-end)
     )
    :modes protobuf-mode
    :working-directory +proto-working-directory
    :predicate +proto--checker-predicate))

(defun +proto-working-directory (&optional arg)
  "Return working directory of the current proto file.

ARG is super ignored."
  (+file-locate-dominting-dir (buffer-file-name) "proto"))

(defun +proto--checker-predicate (&optional arg)
  "Return working directory of the current proto file.

ARG is super ignored."
  buffer-file-name)

(defun +proto--install-package (package &optional force)
  "Install PACKAGE.

Does nothing if the package already exists and FORCE is nil."
  (let* ((gopath (getenv "GOPATH"))
         (package-path (format "%s/src/%s" gopath package)))
    (unless (and (null force) (file-exists-p package-path))
      (shell-command (format "go get -u %s" package)))))

(defun +proto|install-dependencies ()
  "Install dependencies for writing proto files."
  (interactive)
  (message "Install missing dependencies...")
  (seq-do #'+proto--install-package
          '("github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway"
            "github.com/grpc-ecosystem/grpc-gateway/protoc-gen-swagger"
            "github.com/golang/protobuf/protoc-gen-go"))
  (message "All dependencies are installed, have some proto fun!"))


(provide 'init-proto)
;;; init-proto.el ends here
