;;; omnisharp-configs.el --- configs file of omnisharp configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 04 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(use-package omnisharp
  :ensure t
  :defer t
  :init
  (setq omnisharp-server-executable-path "~/.omnisharp/OmniSharp/bin/Debug/OmniSharp.exe")
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'company-mode)
  (add-hook 'csharp-mode-hook 'eldoc-mode)
  (add-hook 'csharp-mode-hook 'd12/omnisharp-setup)
  :config
  (require 'company)
  (d12|diminish omnisharp-mode " ♯")

  (add-to-list 'company-backends 'company-omnisharp)

  (c-set-offset 'case-label '+)
  (c-set-offset 'cpp-macro 'csharp-lineup-if-and-region)

  (local-unset-key (kbd "{"))

  (d12|define-prefix-local "C-c c c" compile csharp-mode-map)
  (d12|define-prefix-local "C-c c f" file csharp-mode-map)
  (d12|define-prefix-local "C-c c p" projectile csharp-mode-map)
  (d12|define-prefix-local "C-c c g" navigation csharp-mode-map)
  (d12|define-prefix-local "C-c c h" documentation csharp-mode-map)
  (d12|define-prefix-local "C-c c r" refactoring csharp-mode-map)
  (d12|define-prefix-local "C-c c s" server csharp-mode-map)
  (d12|define-prefix-local "C-c c t" tests csharp-mode-map)

  (bind-keys
   :map csharp-mode-map
   ;; Compile
   ("C-c c c"   . omnisharp-build-in-emacs)
   ("C-c c f a" . omnisharp-add-to-solution-current-file)
   ("C-c c f A" . omnisharp-add-to-solution-dired-selected-files)
   ("C-c c f r" . omnisharp-remove-from-project-current-file)
   ("C-c c f R" . omnisharp-remove-from-project-dired-selected-files)
   ("C-c c p l" . omnisharp-add-reference)
   ;; Navigation
   ("C-c c g t" . omnisharp-navigate-to-current-file-member)
   ("C-c c g g" . d12/omnisharp-go-to-definition-at-center)
   ("C-c c g G" . omnisharp-go-to-definition-other-window)
   ("C-c c g u" . omnisharp-helm-find-usages)
   ("C-c c g s" . omnisharp-helm-find-symbols)
   ("C-c c g i" . omnisharp-find-implementations)
   ("C-c c g r" . omnisharp-navigate-to-region)
   ("C-c c g m" . omnisharp-navigate-to-solution-member)
   ("C-c c g M" . omnisharp-navigate-to-solution-member-other-window)
   ("C-c c g f" . omnisharp-navigate-to-solution-file)
   ("C-c c g F" . omnisharp-navigate-to-solution-file-then-file-member)
   ;; Help, documentation, info
   ("C-c c h t" . omnisharp-current-type-information)
   ("C-c c h T" . omnisharp-current-type-information-to-kill-ring)
   ;; Refactoring
   ("C-c c r m" . omnisharp-rename)
   ("C-c c r r" . omnisharp-run-code-action-refactoring)
   ;; Server manipulation, inspired spacemacs REPL bindings since C# does not provice a REPL
   ("C-c c s s" . omnisharp-start-omnisharp-server)
   ("C-c c s S" . omnisharp-stop-server)
   ("C-c c s r" . omnisharp-reload-solution)
   ;; Tests
   ("C-c c t a" . omnisharp-unit-test-all)
   ("C-c c t b" . omnisharp-unit-test-fixture)
   ("C-c c t t" . omnisharp-unit-test-single)
   ;; Code manipulation
   ("C-c c u"   . omnisharp-auto-complete-overrides)
   ("C-c c i"   . omnisharp-fix-usings)
   ("C-c c ="   . omnisharp-code-format)
   ("C-c <" . hs-hide-block)
   ("C-c >" . hs-show-block)
   ("C-." . omnisharp-auto-complete)))
