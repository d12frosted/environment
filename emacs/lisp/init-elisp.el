;;; init-elisp.el --- Emacs Lisp support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Nov 2019
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

(require 'use-package)

(use-package lispy
  :defer t
  :diminish
  :hook ((emacs-lisp-mode . lispy-mode))
  :config
  (define-key lispy-mode-map (kbd "C-a") '+beginning-of-line))

(use-package eldoc
  :straight (eldoc :type built-in)
  :diminish eldoc-mode)

(use-package flycheck-package
  :defer nil
  :after flycheck
  :commands (flycheck-package-setup)
  :config
  (flycheck-package-setup))

(provide 'init-elisp)
;;; init-elisp.el ends here
