;;; lang/haskell/+intero.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Dec 2018
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

(def-package! intero
  :commands intero-mode
  :init
  (defun +haskell|init-intero ()
    "Initializes `intero-mode' in haskell-mode, unless stack isn't installed.
This is necessary because `intero-mode' doesn't do its own error checks."
    (when (derived-mode-p 'haskell-mode)
      (if (executable-find "stack")
          (intero-mode +1)
        (message "Couldn't find stack. Refusing to enable intero-mode."))))
  (add-hook 'haskell-mode-local-vars-hook #'+haskell|init-intero)
  :config
  (setq haskell-compile-cabal-build-command "stack build --fast")
  ;; TODO lookup/jumper
  ;; (set-lookup-handlers! 'intero-mode :definition #'intero-goto-definition)
  (set-company-backend! 'intero-mode 'intero-company)
  (when (featurep! :feature syntax-checker)
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  (map! :map intero-mode-map
        :localleader
        "t" #'intero-type-at
        "i" #'intero-info
        "l" #'intero-repl-load
        "e" #'intero-repl-eval-region
        "a" #'intero-apply-suggestions))
