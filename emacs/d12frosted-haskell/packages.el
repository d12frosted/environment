;;; packages.el --- d12frosted-haskell layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris <d12frosted@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; Highly inspired by https://github.com/chrisbarrett/spacemacs-layers
;;
;;; License: GPLv3
;;
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst d12frosted-haskell-packages
  '(haskell-mode
    smart-ops
    aggressive-indent
    indent-dwim
    intero
    llvm-mode

    (ghc-dump :location local)
    (haskell-flyspell :location local)
    (haskell-imports :location local)
    (haskell-pragmas :location local)
    (haskell-ret :location local)
    (haskell-unicode :location local)
    (haskell-autoinsert :location local)
    (cb-haskell-alignment :location local)
    (haskell-flycheck-holes :location local)))

(defun d12frosted-haskell/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :config
    (progn
      (setq haskell-process-type 'stack-ghci
            haskell-process-use-presentation-mode t
            haskell-interactive-mode-eval-mode 'haskell-mode
            haskell-interactive-mode-scroll-to-bottom t
            haskell-interactive-popup-errors t
            haskell-interactive-prompt "\nλ> "
            haskell-process-show-debug-tips t
            haskell-stylish-on-save t
            haskell-indentation-layout-offset 2
            haskell-indentation-starter-offset 2
            haskell-indentation-where-pre-offset 2
            haskell-indentation-where-post-offset 2
            haskell-indentation-left-offset 2
            haskell-indent-spaces 2)

      (spacemacs|diminish interactive-haskell-mode "λ" "λ")

      (defun d12frosted-haskell/set-indentation-step ()
        (setq tab-width 2))

      (add-hook 'haskell-mode-hook #'d12frosted-haskell/set-indentation-step)

      ;; Make 3rd-party tools aware of common syntax extensions.

      (setq haskell-language-extensions
            '("-XUnicodeSyntax" "-XLambdaCase" "-XRankNTypes"))

      ;; Ignore generated files.

      (add-to-list 'completion-ignored-extensions ".hi")
      (add-to-list 'completion-ignored-extensions ".gm")

      ;; Disable haskell-interactive-mode for org src blocks.

      (defun d12frosted-haskell/maybe-haskell-interactive-mode ()
        (unless (bound-and-true-p org-src-mode)
          (interactive-haskell-mode)))

      (add-hook 'haskell-mode-hook #'d12frosted-haskell/maybe-haskell-interactive-mode)

      ;; Disable some faces.

      (custom-set-faces
       '(haskell-interactive-face-compile-error ((t (:foreground nil))))
       '(haskell-operator-face ((t :italic nil))))

      ;; Set keybindings.

      (define-key haskell-mode-map (kbd "<backtab>") #'haskell-indentation-indent-backwards)
      (define-key haskell-mode-map (kbd "TAB")       #'haskell-indentation-indent-line)
      (define-key haskell-mode-map (kbd "M-P")       #'flymake-goto-prev-error)
      (define-key haskell-mode-map (kbd "M-N")       #'flymake-goto-next-error)
      (define-key haskell-mode-map (kbd "C-,")       #'haskell-move-nested-left)
      (define-key haskell-mode-map (kbd "C-.")       #'haskell-move-nested-right)
      (define-key haskell-mode-map (kbd "C-c C-d")   #'haskell-w3m-open-haddock)
      (define-key haskell-mode-map (kbd "C-c C-f")   #'haskell-cabal-visit-file)
      (define-key haskell-mode-map (kbd "C-c C-h")   #'haskell-hoogle)

      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "gg"  'haskell-mode-jump-to-def-or-tag
        "gi"  'haskell-navigate-imports
        "f"   'haskell-mode-stylish-buffer

        "sb"  'haskell-process-load-file
        "sc"  'haskell-interactive-mode-clear
        "ss"  'spacemacs/haskell-interactive-bring
        "sS"  'haskell-interactive-switch

        "ca"  'haskell-process-cabal
        "cb"  'haskell-process-cabal-build
        "cc"  'haskell-compile
        "cv"  'haskell-cabal-visit-file

        "hd"  'inferior-haskell-find-haddock
        "hh"  'hoogle
        "hH"  'haskell-hoogle-lookup-from-local
        "hi"  (lookup-key haskell-mode-map (kbd "C-c TAB"))
        "ht"  (lookup-key haskell-mode-map (kbd "C-c C-t"))
        "hT"  'spacemacs/haskell-process-do-type-on-prev-line
        "hy"  'hayoo

        "dd"  'haskell-debug
        "db"  'haskell-debug/break-on-function
        "dn"  'haskell-debug/next
        "dN"  'haskell-debug/previous
        "dB"  'haskell-debug/delete
        "dc"  'haskell-debug/continue
        "da"  'haskell-debug/abandon
        "dr"  'haskell-debug/refresh)))

  (use-package haskell-interactive-mode
    :after haskell-mode
    :config
    (progn
      (define-key haskell-interactive-mode-map (kbd "C-c C-h") #'haskell-hoogle)))

  (use-package haskell-cabal
    :after haskell-mode
    :config
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") #'haskell-interactive-mode-clear))

  (use-package haskell-debug
    :after haskell-mode
    :config
    (progn
      (add-hook 'haskell-debug-mode-hook #'flyspell-mode-off))))

(defun d12frosted-haskell/post-init-aggressive-indent ()
  (with-eval-after-load 'aggressive-indent
    (with-no-warnings
      (add-to-list 'aggressive-indent-excluded-modes 'haskell-interactive-mode))))

(defun d12frosted-haskell/init-intero ()
  (use-package intero
    :after haskell-mode
    :config
    (progn
      (add-hook 'haskell-mode-hook #'intero-mode)
      (spacemacs|diminish intero-mode "λ" "λ")
      (define-key intero-mode-map (kbd "M-.") #'intero-goto-definition)
      (define-key intero-mode-map (kbd "M-,") #'pop-global-mark))))

(defun d12frosted-haskell/post-init-smart-ops ()

  (defun d12frosted-haskell/reformat-comment-at-point ()
    (-when-let ((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
      (when (and (equal op "{")
                 (s-matches? (rx bos "{" (* (any "-" space)) "}" eos)
                             (buffer-substring beg end)))
        (goto-char beg)
        (delete-region beg end)
        (insert "{- ")
        (save-excursion (insert " -}")))))

  (defun d12frosted-haskell/reformat-pragma-at-point ()
    (-when-let ((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
      (when (and (equal op "{")
                 (s-matches? (rx bos "{" (* (any "-" space "#")) "}" eos)
                             (buffer-substring beg end)))
        (goto-char beg)
        (delete-region beg end)
        (insert "{-# ")
        (save-excursion (insert " #-}")))))

  (defun d12frosted-haskell/indent-if-in-exports ()
    (when (ignore-errors (s-matches? "ExportSpec" (elt (shm-current-node) 0)))
      (haskell-indentation-indent-line)))

  (defconst d12frosted-haskell/smart-ops
    (-flatten-n 1
                (list
                 (smart-ops "." :bypass? t)
                 (smart-ops "->" "=>")
                 (smart-ops "$" "=" "~" "^" ":" "?")
                 (smart-ops "^." ".~" "^~" "%~" :pad-before t :pad-after t)
                 (smart-op ";"
                           :pad-before nil :pad-after t)
                 (smart-ops ","
                            :pad-before nil :pad-after t
                            :action
                            #'d12frosted-haskell/indent-if-in-exports)
                 (smart-op "-"
                           :action #'d12frosted-haskell/reformat-comment-at-point)
                 (smart-op "#"
                           :pad-before nil :pad-after nil
                           :action #'d12frosted-haskell/reformat-pragma-at-point)
                 (smart-ops-default-ops))))

  (define-smart-ops-for-mode 'haskell-mode
    d12frosted-haskell/smart-ops)

  (define-smart-ops-for-mode 'haskell-interactive-mode
    (smart-op ":" :pad-unless (lambda (_) (haskell-interactive-at-prompt)))
    d12frosted-haskell/smart-ops))

(defun d12frosted-haskell/post-init-indent-dwim ()
  (use-package indent-dwim
    :config
    (progn
      (autoload 'haskell-unicode-apply-to-buffer "haskell-unicode")

      (defun d12frosted-haskell/format-dwim ()
        "Reformat the buffer."
        (interactive "*")
        (hindent/reformat-decl)
        (haskell-mode-stylish-buffer)
        (haskell-unicode-apply-to-buffer))

      (add-to-list 'indent-dwim-commands-alist '(haskell-mode . d12frosted-haskell/format-dwim)))))

(defun d12frosted-haskell/init-llvm-mode ()
  (use-package llvm-mode))

(defun d12frosted-haskell/init-ghc-dump ()
  (use-package ghc-dump
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "D" #'ghc-dump-popup)
      (bind-key "q" #'cb-buffers-maybe-kill ghc-dump-popup-mode-map))))

(defun d12frosted-haskell/init-haskell-flyspell ()
  (use-package haskell-flyspell
    :commands (haskell-flyspell-init)
    :init (add-hook 'haskell-mode-hook #'haskell-flyspell-init)))

(defun d12frosted-haskell/init-haskell-imports ()
  (use-package haskell-imports
    :commands (haskell-imports-init)
    :init (add-hook 'haskell-mode-hook #'haskell-imports-init)))

(defun d12frosted-haskell/init-haskell-pragmas ()
  (use-package haskell-pragmas
    :commands (haskell-pragmas-init)
    :init (add-hook 'haskell-mode-hook #'haskell-pragmas-init)))

(defun d12frosted-haskell/init-haskell-unicode ()
  (use-package haskell-unicode
    :commands (haskell-unicode-init)
    :init (add-hook 'haskell-mode-hook #'haskell-unicode-init)))

(defun d12frosted-haskell/init-haskell-autoinsert ()
  (use-package haskell-autoinsert
    :commands (haskell-autoinsert-init)
    :config (haskell-autoinsert-init)))

(defun d12frosted-haskell/init-cb-haskell-alignment ()
  (use-package cb-haskell-alignment
    :commands (cb-haskell-alignment-init)
    :init
    (add-hook 'haskell-mode-hook #'cb-haskell-alignment-init)))

(defun d12frosted-haskell/init-haskell-ret ()
  (use-package haskell-ret
    :commands (haskell-ret-init)
    :init (add-hook 'haskell-mode-hook #'haskell-ret-init)))

(defun d12frosted-haskell/init-haskell-flycheck-holes ()
  (use-package haskell-flycheck-holes
    :after (haskell-mode flycheck)
    :init
    (add-hook 'haskell-mode-hook #'haskell-flycheck-holes-init)))

;;; packages.el ends here
