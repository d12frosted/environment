;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

(after! elisp-mode
  ;; Flycheck produces a *lot* of false positives in emacs configs, so
  ;; disable it when you're editing them
  (add-hook 'flycheck-mode-hook #'+emacs-lisp|disable-flycheck-maybe)
  (add-hook! 'emacs-lisp-mode-hook #'(rainbow-delimiters-mode highlight-quoted-mode)))
