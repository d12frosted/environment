;;; init-ruby.el --- ruby is a gem -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Jun 2020
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

(require 'init-package)
(require 'init-lsp)

(use-package ruby-mode
  :defer t
  :straight nil
  :hook
  (ruby-mode . lsp)
  :init
  (setq ruby-insert-encoding-magic-comment nil))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(provide 'init-ruby)
;;; init-ruby.el ends here
