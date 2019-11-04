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

(use-package haskell-mode
  :hook ((haskell-mode . subword-mode)
         (haskell-mode . haskell-collapse-mode))
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

(use-package intero
  :commands intero-mode
  :hook ((haskell-mode . +haskell-init-intero))
  :init
  (defun +haskell-init-intero ()
    "Conditionally initialise `intero-mode'.

To enable `intero-mode', 'stack' must be installed.

This is necessary because `intero-mode' doesn't do its own error
checks."
    (when (derived-mode-p 'haskell-mode)
      (if (executable-find "stack")
          (intero-mode +1)
        (message "Couldn't find stack. Refusing to enable intero-mode."))))
  :config
  (+company-set-backend 'intero-mode 'intero-company)
  (when (fboundp 'flycheck-add-next-checker)
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))

(use-package lsp-haskell
  :disabled
  :after haskell-mode
  :hook ((haskell-mode . lsp)))

(provide 'init-haskell)
;;; init-haskell.el ends here
