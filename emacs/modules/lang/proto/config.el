;;; lang/proto/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Jul 2019
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

(after! protobuf-mode
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
     source-inplace)
    :error-patterns
    ((info line-start (file-name) ":" line ":" column ": note: " (message) line-end)
     (error line-start (file-name) ":" line ":" column ": " (message) line-end)
     (error line-start (file-name) ":" line " " (message) line-end)
     (error line-start (message "In file included from") " " (file-name) ":" line ":" column ":" line-end)
     )
    :modes protobuf-mode
    :working-directory +proto-working-directory
    :predicate +proto--checker-predicate))
