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

      (defun spacemacs/haskell-process-do-type-on-prev-line ()
        (interactive)
        (if haskell-enable-ghci-ng-support
            (haskell-mode-show-type-at 1)
          (haskell-process-do-type 1)))

      ;; key bindings
      (evil-leader/set-key-for-mode 'haskell-mode
        "mgg"  'haskell-mode-jump-to-def-or-tag
        "mf"   'haskell-mode-stylish-buffer

        "msb"  'haskell-process-load-or-reload
        "msc"  'haskell-interactive-mode-clear
        "mss"  'haskell-interactive-bring
        "msS"  'haskell-interactive-switch

        "mca"  'haskell-process-cabal
        "mcb"  'haskell-process-cabal-build
        "mcc"  'haskell-compile
        "mcv"  'haskell-cabal-visit-file

        "mhd"  'inferior-haskell-find-haddock
        "mhh"  'hoogle
        "mhi"  'haskell-process-do-info
        "mht"  'haskell-process-do-type
        "mhT"  'spacemacs/haskell-process-do-type-on-prev-line
        "mhy"  'hayoo

        "mdd"  'haskell-debug
        "mdb"  'haskell-debug/break-on-function
        "mdn"  'haskell-debug/next
        "mdN"  'haskell-debug/previous
        "mdB"  'haskell-debug/delete
        "mdc"  'haskell-debug/continue
        "mda"  'haskell-debug/abandon
        "mdr"  'haskell-debug/refresh)

       ;; Switch back to editor from REPL
      (evil-leader/set-key-for-mode 'haskell-interactive-mode
        "msS"  'haskell-interactive-switch)

      ;; Compile
      (evil-leader/set-key-for-mode 'haskell-cabal
        "mC"  'haskell-compile)

      ;; Cabal-file bindings
      (evil-leader/set-key-for-mode 'haskell-cabal-mode
        ;; "m="  'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
        "md" 'haskell-cabal-add-dependency
        "mb" 'haskell-cabal-goto-benchmark-section
        "me" 'haskell-cabal-goto-executable-section
        "mt" 'haskell-cabal-goto-test-suite-section
        "mm" 'haskell-cabal-goto-exposed-modules
        "ml" 'haskell-cabal-goto-library-section
        "mn" 'haskell-cabal-next-subsection
        "mp" 'haskell-cabal-previous-subsection
        "mN" 'haskell-cabal-next-section
        "mP" 'haskell-cabal-previous-section
        "mf" 'haskell-cabal-find-or-create-source-file)

      ;; Make "RET" behaviour in REPL saner
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return)

      ;; Useful to have these keybindings for .cabal files, too.
      (eval-after-load 'haskell-cabal-mode-map
        '(define-key haskell-cabal-mode-map
           [?\C-c ?\C-z] 'haskell-interactive-switch))
      ))

  (eval-after-load 'haskell-indentation
    '(progn
       ;; Show indentation guides in insert or emacs state only.
       (defun spacemacs//haskell-indentation-show-guides ()
         "Show visual indentation guides."
         (when (and (boundp 'haskell-indentation-mode) haskell-indentation-mode)
           (haskell-indentation-enable-show-indentations)))

       (defun spacemacs//haskell-indentation-hide-guides ()
         "Hide visual indentation guides."
         (when (and (boundp 'haskell-indentation-mode) haskell-indentation-mode)
           (haskell-indentation-disable-show-indentations)))

       ;; first entry in normal state
       (add-hook 'evil-normal-state-entry-hook 'spacemacs//haskell-indentation-hide-guides)

       (add-hook 'evil-insert-state-entry-hook 'spacemacs//haskell-indentation-show-guides)
       (add-hook 'evil-emacs-state-entry-hook 'spacemacs//haskell-indentation-show-guides)
       (add-hook 'evil-insert-state-exit-hook 'spacemacs//haskell-indentation-hide-guides)
       (add-hook 'evil-emacs-state-exit-hook 'spacemacs//haskell-indentation-hide-guides))))

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
