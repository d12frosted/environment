;;; tools/lsp/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Jul 2019
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

(after! lsp-mode
  ;; Don't prompt to restart LSP servers while quitting Emacs
  (add-hook! 'kill-emacs-hook (setq lsp-restart 'ignore)))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-session-file (concat nucleus-etc-dir "lsp-session")
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-groovy-server-install-dir (concat nucleus-etc-dir "groovy-langserver/"))
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable t))

(def-package! company-lsp
  :when (featurep! :completion company)
  :after lsp-mode
  :config
  ;; Make sure that `company-capf' is disabled since it is incompatible with
  ;; `company-lsp' (see lsp-mode#884)
  (setq-hook! 'lsp-mode-hook company-backends
              (cons 'company-lsp
                    (remq 'company-capf company-backends))))
