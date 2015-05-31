;;; haskell-configs.el --- configs file of haskell configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 16 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; todo:
;; * configure `w3m' hackage browsing

;;; Code

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  ;; insert template for new files
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  ;; configure template for new files
  (setq-default haskell-auto-insert-module-format-string
                (concat
                 "{-# LANGUAGE NoImplicitPrelude #-}\n"
                 "{-# LANGUAGE OverloadedStrings #-}\n\n"
                 "module %s where\n\n"))
  :config
  (d12|rename-modeline "haskell-mode" haskell-mode "λ")
  ;; add `speedbar' support
  ;; (speedbar-add-supported-extension ".hs")

  ;; setup `align'
  (require 'align)
  (add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))

  ;; force `structured-haskell-mode' loading
  (require 'shm)

  ;; enable `haskell-interactive-mode'
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  ;; some custom variables
  (custom-set-variables
   '(haskell-process-type 'cabal-repl)
   '(haskell-process-args-cabal-repl
     '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
   '(haskell-notify-p t)
   '(haskell-stylish-on-save nil)
   '(haskell-tags-on-save nil)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-reload-with-fbytecode nil)
   '(haskell-process-use-presentation-mode t)
   '(haskell-interactive-mode-include-file-name nil)
   '(haskell-interactive-mode-eval-pretty nil)
   '(shm-use-presentation-mode t)
   '(shm-auto-insert-skeletons t)
   '(shm-auto-insert-bangs t)
   '(haskell-process-suggest-haskell-docs-imports t)
   '(hindent-style "chris-done")
   '(haskell-interactive-mode-eval-mode 'haskell-mode)
   '(haskell-process-path-ghci "ghci-ng")
   '(haskell-process-args-ghci '("-ferror-spans"))
   '(haskell-process-generate-tags nil)
   '(haskell-complete-module-preferred
     '("Data.Text"
       "Data.ByteString"
       "Data.ByteString.Lazy"
       "Data.Conduit"
       "Data.Function"
       "Data.List"
       "Data.Map"
       "Data.Maybe"
       "Data.Monoid"
       "Data.Ord")))

  (bind-keys
   :map haskell-mode-map
   ("M-." . haskell-mode-jump-to-def-or-tag)
   ("M-," . pop-tag-mark)
   ("C-c C-l" . haskell-process-load-or-reload)
   ("C-`" . haskell-interactive-bring)
   ("C-c C-t" . haskell-process-do-type)
   ("C-c C-i" . haskell-process-do-info)
   ("C-c C-c" . haskell-process-cabal-build)
   ("C-c C-k" . haskell-interactive-mode-clear)
   ("C-c c" . haskell-process-cabal)
   ("SPC" . haskell-mode-contextual-space)
   ("C-<return>" . haskell-simple-indent-newline-indent)
   ("C-<right>" . haskell-move-right)
   ("C-<left>" . haskell-move-left))

  (bind-keys
   :map haskell-interactive-mode-map
   ("<f5>" . haskell-process-load-or-reload)
   ("C-c C-k" . haskell-interactive-mode-clear)
   ("C-c C-c" . haskell-process-cabal-build)
   ("C-c c" . haskell-process-cabal)
   ("C-<left>" . haskell-interactive-mode-error-backward)
   ("C-<right>" . haskell-interactive-mode-error-forward)
   ("C-c C-i" . haskell-process-do-info)
   ("C-c c" . haskell-process-cabal))

  (bind-keys
   :map haskell-cabal-mode-map
   ("C-`" . haskell-interactive-bring)
   ("C-c C-k" . haskell-interactive-mode-clear)
   ("C-c C-c" . haskell-process-cabal-build)
   ("C-c c" . haskell-process-cabal)))

;;; Structured Haskell Mode
;; =========================

(defvar d12/shm-current-face-bg "#F6FECD")
(defvar d12/shm-quarantine-face-bg "#FBE3E4")

(use-package shm
  :load-path "packages/structured-haskell-mode/elisp/"
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)

  ;; there is no point in `hl-line-mode' when
  ;; `structured-haskell-mode' is enabled
  ;; so disable `hl-line-mode'
  (add-hook 'structured-haskell-mode-hook 'd12/disable-hl-line-mode)
  :config
  (set-face-background 'shm-current-face d12/shm-current-face-bg)
  (set-face-background 'shm-quarantine-face d12/shm-quarantine-face-bg)
  (bind-keys
   :map shm-map
   ("C-c C-p" . shm/expand-pattern)
   ("C-c C-s" . shm/case-split)
   ("SPC" . shm-contextual-space)
   ("C-\\" . shm/goto-last-point)
   ("C-c C-f" . shm-fold-toggle-decl)
   ("C-c i" . shm-reformat-decl)))
