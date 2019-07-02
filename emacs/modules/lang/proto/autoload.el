;;; lang/proto/autoload.el -*- lexical-binding: t; -*-
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

;;;###autoload
(defun +proto-working-directory (&optional arg)
  "Return working directory of the current proto file."
  (+file-locate-dominting-dir (buffer-file-name) "proto"))

;;;###autoload
(defun +proto--checker-predicate (&optional arg)
  "Return working directory of the current proto file."
  buffer-file-name)
