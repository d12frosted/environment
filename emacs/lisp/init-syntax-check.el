;;; init-syntax-check.el --- syntax checking -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
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

(use-package flycheck
  :diminish
  :commands (flycheck-list-errors flycheck-buffer)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :config
  (setq flycheck-global-modes '(not org-mode))
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (global-flycheck-mode +1))

(provide 'init-syntax-check)
;;; init-syntax-check.el ends here
