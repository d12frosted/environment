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
  :commands (global-corfu-mode)
  :init
  (global-corfu-mode)
  (setq-default
   corfu-cycle t))

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
  (lsp-mode . lsp-lens-mode)
  :init
  (setq-default
   lsp-session-file (concat path-etc-dir "lsp-session")
   lsp-auto-guess-root nil
   lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

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

(provide 'init-ide)
;;; init-ide.el ends here
