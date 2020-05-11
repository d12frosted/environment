;;; init-navigation.el --- navigation related configurations -*- lexical-binding: t; -*-
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

(require 'init-keybindings)

(use-package avy
  :defer t)

(use-package ace-link
  :defer t)

(provide 'init-navigation)
;;; init-navigation.el ends here
