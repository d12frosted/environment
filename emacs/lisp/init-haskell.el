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

(use-package haskell-mode
  :config
  (setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
        haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-show-overlays nil) ; flycheck makes this unnecessary
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook #'subword-mode)
  (add-hook 'haskell-mode-hook #'haskell-collapse-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-to-list 'completion-ignored-extensions ".hi"))

(use-package intero
  :commands intero-mode
  :init
  (add-hook 'haskell-mode-local-vars-hook #'+haskell|init-intero)
  :config
  (setq haskell-compile-cabal-build-command "stack build --fast")
  (+company-set-backend 'intero-mode 'intero-company)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(defun +haskell|init-intero ()
  "Initializes `intero-mode' in haskell-mode, unless stack isn't installed.

This is necessary because `intero-mode' doesn't do its own error
checks."
  (when (derived-mode-p 'haskell-mode)
    (if (executable-find "stack")
        (intero-mode +1)
      (message "Couldn't find stack. Refusing to enable intero-mode."))))

(use-package lsp-haskell
  :disabled
  :after haskell-mode
  :init (add-hook 'haskell-mode-hook #'lsp)
  :config
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))

(provide 'init-haskell)
;;; init-haskell.el ends here
