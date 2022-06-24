;;; init-haskell.el --- Haskell support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Haskell support.
;;
;;; Code:

(defconst haskell-backend 'lsp-ui
  "Backend for Haskell IDE: eglot, lsp-ui, dante.")

(use-package haskell-mode
  :hook ((haskell-mode . subword-mode))
  :init
  (when (eq haskell-backend 'eglot)
    (with-eval-after-load 'eglot
      (when (boundp 'eglot-server-programs)
        (add-to-list 'eglot-server-programs
                     '(haskell-mode . ("ghcide" "--lsp"))))))
  :config
  (file-templates-set
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
  :if (eq haskell-backend 'dante)
  :commands (dante-mode
             dante-company)
  :hook (haskell-mode . dante-mode)
  :config
  (setq dante-methods '(stack))
  (when (fboundp 'flycheck-add-next-checker)
    (flycheck-add-next-checker 'haskell-dante
                               '(warning . haskell-hlint))))

(use-package lsp-haskell
  :if (eq haskell-backend 'lsp-ui)
  :after haskell-mode
  :hook ((haskell-mode . lsp))
  :commands (lsp-haskell--hie-command)
  :init
  (setq-default
   lsp-haskell-importlens-on nil))

(provide 'init-haskell)
;;; init-haskell.el ends here
