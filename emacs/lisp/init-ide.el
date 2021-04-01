;;; init-ide.el --- IDE like features -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
;; `company'.
;;
;;; Code:

(require 'config-path)
(require 'init-ui)

(use-package company
  :defer 2
  :diminish
  :hook (after-init . global-company-mode)
  :defines (company-backends)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line
             global-company-mode)
  :init
  (setq-default
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never
   company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode)
   company-backends '(company-capf)
   company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend)))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :defines (prescient-save-file)
  :commands (prescient-persist-mode)
  :config
  (setq prescient-save-file (concat path-cache-dir
                                    "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package flycheck
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
  :hook
  (lsp-mode . lsp-lens-mode)
  :init
  (setq-default
   lsp-session-file (concat path-etc-dir "lsp-session")
   lsp-auto-guess-root nil
   lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(use-package posframe)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package eglot
  :defer t
  :defines (eglot-server-programs))

(provide 'init-ide)
;;; init-ide.el ends here
