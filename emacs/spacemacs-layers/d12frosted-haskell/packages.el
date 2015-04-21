;;; packages.el --- d12frosted-haskell Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-haskell-packages
  '(haskell-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-haskell-excluded-packages '()
  "List of packages to exclude.")

(defvar d12frosted-haskell/packages-dir "~/.environment/emacs/packages")

(defun d12frosted-haskell/init-haskell-mode ()
  "Initialize omnisharp package."
  (use-package haskell-mode
    :defer t
    :init
    :config

    (require 'shm)
    (require 'hindent)
    (require 'shm-case-split)
    (require 'shm-reformat)
    (require 'haskell-mode)
    (require 'hindent)
    (require 'haskell-process)
    (require 'haskell-simple-indent)
    (require 'haskell-interactive-mode)
    (require 'haskell)
    (require 'haskell-font-lock)
    (require 'haskell-debug)

    (custom-set-variables
     '(haskell-process-type 'cabal-repl)
     '(haskell-process-args-ghci '())
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
     '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
     '(shm-use-hdevtools t)
     '(shm-use-presentation-mode t)
     '(shm-auto-insert-skeletons t)
     '(shm-auto-insert-bangs t)
     '(haskell-process-show-debug-tips nil)
     '(haskell-process-suggest-hoogle-imports nil)
     '(haskell-process-suggest-haskell-docs-imports t)
     '(hindent-style "chris-done"))

    (setq haskell-complete-module-preferred
          '("Data.ByteString"
            "Data.ByteString.Lazy"
            "Data.Conduit"
            "Data.Function"
            "Data.List"
            "Data.Map"
            "Data.Maybe"
            "Data.Monoid"
            "Data.Text"
            "Data.Ord"))

    (setq haskell-interactive-mode-eval-mode 'haskell-mode)

    (setq haskell-process-path-ghci
          "ghci-ng")

    (setq haskell-process-args-ghci '("-ferror-spans"))

    (setq haskell-process-args-cabal-repl
          '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))

    (setq haskell-process-generate-tags nil)

    (setq haskell-import-mapping
          '(("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)
")
            ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT
")
            ("Data.ByteString" . "import qualified Data.ByteString as S
import Data.ByteString (ByteString)
")
            ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L
")
            ("Data.Map" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
            ("Data.Map.Strict" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
            ("Data.Set" . "import qualified Data.Set as S
import Data.Set (Set)
")
            ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)
")))

    (setq haskell-language-extensions '())

    ;; Add hook

    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)
    (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
    (add-hook 'w3m-display-hook 'w3m-haddock-display)

    ;; Keybindings

    (define-key highlight-uses-mode-map (kbd "C-t") 'highlight-uses-mode-replace)

    ;; (define-key ghci-script-mode-map (kbd "C-`") 'haskell-interactive-bring)
    ;; (define-key ghci-script-mode-map (kbd "C-c C-l") 'ghci-script-mode-load)
    ;; (define-key ghci-script-mode-map [f5] 'ghci-script-mode-load)
    ;; (define-key ghci-script-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    ;; (define-key ghci-script-mode-map (kbd "C-c c") 'haskell-process-cabal)

    (define-key interactive-haskell-mode-map [f5] 'haskell-process-load-or-reload)
    (define-key interactive-haskell-mode-map [f12] 'turbo-devel-reload)
    (define-key interactive-haskell-mode-map [f12] 'haskell-process-cabal-build-and-restart)
    (define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
    (define-key interactive-haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
    (define-key interactive-haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
    (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
    (define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
    (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

    ;; (define-key hamlet-mode-map [f12] 'haskell-process-cabal-build-and-restart)
    ;; (define-key hamlet-mode-map (kbd "C-`") 'haskell-interactive-bring)
    ;; (define-key hamlet-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

    ;; (define-key html-mode-map [f12] 'haskell-process-cabal-build-and-restart)
    ;; (define-key html-mode-map (kbd "C-`") 'haskell-interactive-bring)
    ;; (define-key html-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

    ;; (define-key css-mode-map [f12] 'haskell-process-cabal-build-and-restart)
    ;; (define-key css-mode-map (kbd "C-`") 'haskell-interactive-bring)

    (define-key haskell-mode-map (kbd "C-c i") 'hindent/reformat-decl)
    (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
    (define-key haskell-mode-map (kbd "-") 'smart-hyphen)
    (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
    (define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
    (define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
    (define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
    (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
    (define-key haskell-mode-map (kbd "C-<right>") 'haskell-move-right)
    (define-key haskell-mode-map (kbd "C-<left>") 'haskell-move-left)
    (define-key haskell-mode-map (kbd "<space>") 'haskell-mode-contextual-space)

    (define-key haskell-cabal-mode-map [f9] 'haskell-interactive-mode-visit-error)
    (define-key haskell-cabal-mode-map [f11] 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map [f12] 'haskell-process-cabal-build-and-restart)
    (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
    (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

    (define-key haskell-interactive-mode-map (kbd "C-c C-v") 'haskell-interactive-toggle-print-mode)
    (define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-interactive-mode-map [f9] 'haskell-interactive-mode-visit-error)
    (define-key haskell-interactive-mode-map [f11] 'haskell-process-cabal-build)
    (define-key haskell-interactive-mode-map [f12] 'haskell-process-cabal-build-and-restart)
    (define-key haskell-interactive-mode-map (kbd "C-<left>") 'haskell-interactive-mode-error-backward)
    (define-key haskell-interactive-mode-map (kbd "C-<right>") 'haskell-interactive-mode-error-forward)
    (define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal)

    (define-key shm-repl-map (kbd "TAB") 'shm-repl-tab)
    (define-key shm-map (kbd "C-c C-p") 'shm/expand-pattern)
    (define-key shm-map (kbd ",") 'shm-comma-god)
    (define-key shm-map (kbd "C-c C-s") 'shm/case-split)
    (define-key shm-map (kbd "SPC") 'shm-contextual-space)
    (define-key shm-map (kbd "C-\\") 'shm/goto-last-point)
    (define-key shm-map (kbd "C-c C-f") 'shm-fold-toggle-decl)
    (define-key shm-map (kbd "C-c i") 'shm-reformat-decl)

    ;; (define-key ide-backend-mode-map [f5] 'ide-backend-mode-load)
    ;; (setq ide-backend-mode-cmd "cabal")
    ))
