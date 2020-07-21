;;; init-go.el --- GO support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 21 Jul 2020
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

(use-package go-mode
  :hook ((go-mode . lsp)))

(provide 'init-go)
;;; init-go.el ends here
