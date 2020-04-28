;;; init-yaml.el --- YAML support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 28 Apr 2020
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

(use-package yaml-mode
  :defer t
  :defines (yaml-indent-offset)
  :hook
  (yaml-mode . lsp)
  :init
  (setq-default yaml-indent-offset tab-width)
  :config)

(provide 'init-yaml)
;;; init-yaml.el ends here
