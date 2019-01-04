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

;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)

(after! elisp-mode
  (set-repl-handler! 'emacs-lisp-mode #'+emacs-lisp/repl)
  (set-eval-handler! 'emacs-lisp-mode #'+emacs-lisp-eval)

  ;; Flycheck produces a *lot* of false positives in emacs configs, so
  ;; disable it when you're editing them
  (add-hook 'flycheck-mode-hook #'+emacs-lisp|disable-flycheck-maybe)
  (add-hook! 'emacs-lisp-mode-hook #'(rainbow-delimiters-mode highlight-quoted-mode)))
