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

;;
;; elisp-mode deferral hack

;; `elisp-mode' is loaded at startup. In order to lazy load its config
;; we need to pretend it isn't loaded
(delq 'elisp-mode features)

;; ...until the first time `emacs-lisp-mode' runs
(advice-add #'emacs-lisp-mode :before #'+emacs-lisp|init)

(defun +emacs-lisp|init (&rest _)
  ;; Some plugins (like yasnippet) run `emacs-lisp-mode' early, to
  ;; parse some elisp. This would prematurely trigger this
  ;; function. In these cases, `emacs-lisp-mode-hook' is let-bound to
  ;; nil or its hooks are delayed, so if we see either, keep
  ;; pretending elisp-mode isn't loaded.
  (when (and emacs-lisp-mode-hook (not delay-mode-hooks))
    ;; Otherwise, announce to the world elisp-mode has been loaded, so `after!'
    ;; handlers can respond and configure elisp-mode as expected.
    (provide 'elisp-mode)
    (advice-remove #'emacs-lisp-mode #'+emacs-lisp|init)))
