;;; ui/theming/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Nov 2018
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

(def-package! leuven-theme
  :init
  (unless +modern-theme
    (setq +modern-theme 'leuven))
  (add-hook '+modern-init-ui-hook #'+leuven|patch-faces)
  (+modern-mark-solaire-theme 'leuven))
