;;; init-ide.el --- IDE like features -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 08 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Emacs can be an IDE. Sort of, thanks to `flycheck', `lsp-mode' and
;; `corfu'.
;;
;;; Code:

(require 'config-path)

(use-package corfu
  :ensure t
  :commands (global-corfu-mode
             corfu-history-mode
             corfu-popupinfo-mode)
  :init
  (setq-default
   corfu-cycle t
   corfu-auto t
   corfu-auto-prefix 2
   corfu-auto-delay 0
   corfu-popupinfo-delay '(0.5 . 0.2)
   corfu-preview-current 'insert
   corfu-preselect 'prompt
   corfu-on-exact-match nil)
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of
  ;; `completion-at-point-functions' which is used by
  ;; `completion-at-point'. The order of the functions matters, the
  ;; first function returning a result wins. Note that the list of
  ;; buffer-local completion functions takes precedence over the
  ;; global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package flycheck
  :ensure t
  :defer 1
  :commands (global-flycheck-mode)
  :init
  (setq-default
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-check-syntax-automatically
   '(save idle-change mode-enabled)
   flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode +1))

;; See lisp/dash-functional.el for more information. Here we simply
;; make sure that our mock is loaded instead of upstream.
(eval-when-compile
  (let ((dir (expand-file-name "lisp" path-emacs-dir)))
    (delete dir load-path)
    (add-to-list 'load-path dir)))

(use-package lsp-mode
  :ensure t
  :hook
  ((lsp-mode . lsp-diagnostics-mode)
   (lsp-mode . lsp-enable-which-key-integration)
   ;; (lsp-mode . lsp-lens-mode)
   ((tsx-ts-mode
     typescript-ts-mode
     js-ts-mode)
    . lsp-deferred))
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq-default
   lsp-session-file (concat path-etc-dir "lsp-session")
   lsp-completion-provider :none ; using corfu
   lsp-diagnostics-provider :flycheck
   lsp-log-io nil ; use only for debugging as it drastically affects performance
   lsp-keep-workspace-alive nil ; close LSP server if all project buffers are closed
   lsp-idle-delay 0.5
   lsp-enable-xref t
   lsp-auto-configure t
   ;; lsp-auto-guess-root nil
   lsp-eldoc-enable-hover t
   lsp-enable-dap-auto-configure t
   lsp-enable-file-watchers nil
   lsp-enable-folding nil
   lsp-enable-imenu t
   lsp-enable-indentation nil
   lsp-enable-links nil
   lsp-enable-on-type-formatting nil
   lsp-enable-suggest-server-download t
   lsp-enable-symbol-highlighting t
   lsp-enable-text-document-color nil

   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-diagnostic-max-lines 20

   lsp-completion-enable t
   lsp-completion-enable-additional-text-edit t
   lsp-enable-snippet t
   lsp-completion-show-kind t

   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-enable-diagnostics nil
   lsp-headerline-breadcrumb-enable-symbol-numbers nil
   lsp-headerline-breadcrumb-icons-enable nil

   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-workspace-status-enable nil
   lsp-signature-doc-lines 1
   lsp-ui-doc-use-childframe t
   lsp-eldoc-render-all nil
   lsp-lens-enable nil

   lsp-semantic-tokens-enable nil
   lsp-use-plists t)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode
             lsp-ui-doc-show
             lsp-ui-doc-glance)
  :after (lsp-mode)
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-position 'at-point))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (go-mode . go-ts-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

(use-package posframe
  :ensure t)

(use-package eglot
  :ensure t
  :defer t
  :defines (eglot-server-programs))

(use-package consult-lsp
  :ensure t
  :defer t
  :after lsp
  :config
  (define-key
    lsp-mode-map
    [remap xref-find-apropos]
    #'consult-lsp-symbols))

(defun ide-treesit-install-grammars ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
             (bash "https://github.com/tree-sitter/tree-sitter-bash")
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
             (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             (make "https://github.com/alemuller/tree-sitter-make")
             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
             (cmake "https://github.com/uyha/tree-sitter-cmake")
             (c "https://github.com/tree-sitter/tree-sitter-c")
             (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
             (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (ide-treesit-install-grammars))

(use-package apheleia
  :ensure t
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

(provide 'init-ide)
;;; init-ide.el ends here
