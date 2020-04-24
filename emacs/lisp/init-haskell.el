;;; init-haskell.el --- Haskell support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Nov 2019
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

(require 'init-completion)
(require 'init-file-templates)
(require 'init-lsp)

(defconst +haskell-backend 'eglot
  "Backend for Haskell IDE: eglot, lsp-ui, dante.")

(use-package haskell-mode
  :hook ((haskell-mode . subword-mode))
  :init
  (when (eq +haskell-backend 'eglot)
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp")))))
  :config
  (+file-templates-set
    'haskell-mode
    :trigger "__module"
    :project t)
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)
  ;; flycheck makes this unnecessary
  (setq haskell-process-show-overlays nil)
  (add-to-list 'completion-ignored-extensions ".hi"))

(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode))

(use-package dante
  :if (eq +haskell-backend 'dante)
  :commands (dante-mode
             dante-company)
  :hook (haskell-mode . dante-mode)
  :config
  (setq dante-methods '(stack))
  (+company-set-backend 'dante-mode #'dante-company)
  (when (fboundp 'flycheck-add-next-checker)
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))))

(use-package lsp-haskell
  :if (eq +haskell-backend 'lsp-ui)
  :after haskell-mode
  :hook ((haskell-mode . lsp))
  :commands (lsp-haskell--hie-command)
  :init
  (setq lsp-haskell-process-path-hie "ghcide"
        lsp-haskell-process-args-hie nil))

(provide 'init-haskell)
;;; init-haskell.el ends here
