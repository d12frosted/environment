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

(use-package haskell-mode
  :hook ((haskell-mode . subword-mode))
  :config
  (+file-templates-set
    'haskell-mode
    :trigger "__module"
    :project t)
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)
  ;; flycheck makes this unnecessary
  (setq haskell-process-show-overlays nil)
  (setq haskell-stylish-on-save t)
  (add-to-list 'completion-ignored-extensions ".hi"))

(use-package dante
  :disabled
  :commands dante-mode
  :hook (haskell-mode-local-vars . dante-mode)
  :config
  (+company-set-backend 'dante-mode #'dante-company)
  (when (fboundp 'flycheck-add-next-checker)
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))))

(use-package lsp-haskell
  :after haskell-mode
  :hook ((haskell-mode . lsp))
  :commands (lsp-haskell--hie-command)
  :init
  (eval-after-load 'lsp
    '(lsp-register-client
      (make-lsp--client
       :new-connection (lsp-stdio-connection (lambda () (lsp-haskell--hie-command)))
       :major-modes '(haskell-mode)
       :server-id 'hie
       ;; :multi-root t
       :initialization-options 'lsp-haskell--make-init-options))))

(provide 'init-haskell)
;;; init-haskell.el ends here
