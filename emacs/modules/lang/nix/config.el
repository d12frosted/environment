;;; lang/nix/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 18 Dec 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(def-package! nix-mode
  :mode "\\.nix\\'"
  :config
  (set-company-backend! 'nix-mode 'company-nixos-options))

(def-package! nix-drv-mode
  :mode "\\.drv\\'")

(def-package! nix-update
  :commands nix-update-fetch)

(def-package! nix-repl
  :commands nix-repl-show)
