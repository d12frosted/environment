;;; init-lsp.el --- LSP support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 05 Dec 2019
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

(require 'init-path)
(require 'init-package)

(use-package lsp-mode
  :defines (lsp-session-file)
  :hook
  (lsp-mode . lsp-lens-mode)
  :init
  (setq lsp-session-file (concat +path-etc-dir "lsp-session")
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil))

(use-package company-lsp
  :defer t
  :commands company-lsp)

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(use-package posframe)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package lsp-treemacs
  :defer t
  :defines (lsp-metals-treeview-show-when-views-received
            lsp-metals-treeview-logging)
  :commands (lsp-metals-treeview-enable)
  :init
  (setq lsp-metals-treeview-show-when-views-received t
        lsp-metals-treeview-logging t)
  :config
  (lsp-metals-treeview-enable t))

(use-package eglot
  :defer t)

(provide 'init-lsp)
;;; init-lsp.el ends here
