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
(require 'init-package)
(require 'init-syntax-check)
(require 'init-completion)
(require 'init-ctags)

(defvar-local +proto-custom-include-path nil
  "Path to custom library for protobuf checker.")

(defun +proto--checker-predicate (&optional _)
  "Enable proto checker only when buffer has associated file."
  buffer-file-name)

(use-package protobuf-mode
  :defer t
  :after flycheck
  :hook ((protobuf-mode . subword-mode)
         (protobuf-mode . +ctags-enable-auto-update))
  :config
  (add-hook 'protobuf-mode-hook #'+proto|install-dependencies)
  (+company-set-backend 'protobuf-mode '(company-dabbrev company-ctags))
  (define-key protobuf-mode-map (kbd "M-.") #'counsel-etags-find-tag-at-point)
  (require 'company-ctags)
  (add-to-list 'company-ctags-modes 'protobuf-mode)
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
     (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
     (error line-start (file-name) ":" line ":" column ": " (message) line-end)
     (error line-start (file-name) ": " (message) line-end)
     (error line-start (message "In file included from") " " (file-name) ":" line ":" column ":" line-end)
     )
    :modes protobuf-mode
    :working-directory +proto-working-directory
    :predicate +proto--checker-predicate))

(defun +proto-working-directory (&optional _)
  "Return working directory of the current proto file."
  (+file-locate-dominting-dir (buffer-file-name) "proto"))

(defun +proto--package-path (package)
  "Calculate location of PACKAGE."
  (format "%s/src/%s/" (getenv "GOPATH") package))

(defun +proto--install-package (package &optional force)
  "Install PACKAGE.

Does nothing if the package already exists and FORCE is nil.

Returns path to generated TAGS file."
  (let* ((package-path (+proto--package-path package))
         (tags-file (expand-file-name +ctags-file-name package-path)))
    (unless (and (null force) (file-exists-p package-path))
      (shell-command (format "go get -u %s" package)))
    (unless (and (null force) (file-exists-p tags-file))
      (+ctags-create package-path "protobuf"))
    tags-file))

(defun +proto|install-dependencies ()
  "Install dependencies for writing proto files."
  (interactive)
  (message "Install missing dependencies...")
  (setq-local
   tags-table-list
   (append
    tags-table-list
    (list (counsel-etags-locate-tags-file))
    (seq-map #'+proto--install-package
             '("github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway"
               "github.com/grpc-ecosystem/grpc-gateway/protoc-gen-swagger"
               "github.com/golang/protobuf/protoc-gen-go"
               "github.com/protocolbuffers/protobuf"))))
  (setq-local counsel-etags-extra-tags-files
              tags-table-list)
  (setq-local company-ctags-extra-tags-files
              tags-table-list)
  (message "All dependencies are installed, have some proto fun!"))

(provide 'init-proto)
;;; init-proto.el ends here
