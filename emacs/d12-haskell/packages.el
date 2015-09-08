;;; packages.el --- d12-haskell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Boris Buliga & Contributors
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq d12-haskell-packages
      '(
        haskell-mode
        shm
        company
        company-ghci
        flycheck
        flycheck-haskell
        ))

(defun d12-haskell/init-haskell-mode ()
  "Initialize haskell-mode."
  (use-package haskell-mode
    :defer t
    :init
    :config
    (progn
      (defun d12/init-haskell-mode ()
        (if (fboundp 'electric-indent-local-mode)
            (electric-indent-local-mode -1))
        (when haskell-enable-shm-support
          (setq-local global-hl-line-mode nil)))

      (add-hook 'haskell-mode-hook 'd12/init-haskell-mode)
      (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
      (add-hook 'haskell-mode-hook 'haskell-doc-mode)
      (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
      (add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

      (unless haskell-enable-shm-support
        (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

      (spacemacs|diminish interactive-haskell-mode " λ")

      (setq haskell-notify-p t
            haskell-tags-on-save nil
            haskell-interactive-popup-error nil
            haskell-process-suggest-remove-import-lines nil
            haskell-process-auto-import-loaded-modules t
            haskell-stylish-on-save nil
            haskell-process-log t
            haskell-process-reload-with-fbytecode nil
            haskell-process-use-presentation-mode t
            haskell-interactive-mode-include-file-name nil
            haskell-interactive-mode-eval-pretty nil
            haskell-process-suggest-haskell-docs-imports nil
            hindent-style haskell-enable-hindent-style
            haskell-interactive-mode-eval-mode 'haskell-mode))))

(when haskell-enable-shm-support
  (defun d12-haskell/init-shm ()
    "Initialize structured haskell mode."
    (use-package shm
      :init
      (add-hook 'haskell-mode-hook 'structured-haskell-mode))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun d12-haskell/post-init-company ()
    (spacemacs|add-company-hook haskell-mode)
    (spacemacs|add-company-hook haskell-interactive-mode)
    ))

(defun d12-haskell/init-company-ghci ()
  (use-package company-ghci
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (push 'company-ghci company-backends-haskell-mode)
    (push 'company-ghci company-backends-haskell-interactive-mode)))

;; Currently flycheck breaks haskell interactive mode

;; (defun d12frosted/post-init-flycheck ()
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))

;; (when (configuration-layer/layer-usedp 'syntax-checking)
;;   (defun d12frosted/init-flycheck-haskell ()
;;     (use-package flycheck-haskell
;;       :if (configuration-layer/package-usedp 'flycheck)
;;       :commands flycheck-haskell-configure
;;       :init (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))))
