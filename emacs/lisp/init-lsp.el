;;; init-lsp.el --- LSP support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 05 Dec 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-path)
(require 'init-package)

(use-package lsp-mode
  :defer t
  :defines (lsp-session-file)
  :init
  (setq lsp-session-file (concat +path-etc-dir "lsp-session")
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-prefer-flymake nil))

(use-package company-lsp
  :defer t
  :commands company-lsp)

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
