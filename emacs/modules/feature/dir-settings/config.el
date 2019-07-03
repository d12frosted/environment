;;; feature/dir-settings/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Jul 2019
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

(add-hook 'find-file-hook '+dir-settings/load)

(after! elisp-mode
  (when (featurep! :lang emacs-lisp)
    (add-to-list '+emacs-lisp-flycheck-iniquity-files +dir-settings-filename)))
