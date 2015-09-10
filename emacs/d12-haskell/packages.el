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
        hindent
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
      (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
      (add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

      (unless haskell-enable-shm-support
        (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

      (spacemacs|diminish interactive-haskell-mode " λ")

      (setq
       ;; Use notify.el (if you have it installed) at the end of running
       ;; Cabal commands or generally things worth notifying.
       haskell-notify-p t
       ;; Remove annoying error popups
       haskell-interactive-popup-error nil
       ;; Better import handling
       haskell-process-suggest-remove-import-lines t
       haskell-process-auto-import-loaded-modules t
       ;; Disable haskell-stylish on save, it breaks flycheck highlighting
       haskell-stylish-on-save nil
       haskell-interactive-mode-eval-mode 'haskell-mode)

(defun d12-haskell/init-hindent ()
  (use-package hindent
    :defer t
    :if (stringp haskell-enable-hindent-style)
    :init
    (add-hook 'haskell-mode-hook #'hindent-mode)
    :config
    (progn
      (setq hindent-style haskell-enable-hindent-style)
      (evil-leader/set-key-for-mode 'haskell-mode
        "mF" 'hindent/reformat-decl))))
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
