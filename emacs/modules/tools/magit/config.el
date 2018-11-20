;;; tools/magit/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

(def-package! magit
  :init
  ;; TODO: we already use `global-auto-revert-mode'
  (setq magit-auto-revert-mode nil))

(def-package! magit-todos
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-require-colon nil))
