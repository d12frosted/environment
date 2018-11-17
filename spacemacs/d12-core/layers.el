;;; layers.el --- d12-core layer layers file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(configuration-layer/declare-layers
 '(better-defaults
   ivy
   spacemacs-completion
   spacemacs-editing
   spacemacs-layouts
   spacemacs-project
   spacemacs-navigation
   (auto-completion :disabled-for org spacemacs-org)
   (syntax-checking :variables syntax-checking-enable-tooltips nil)))

;;; layers.el ends here
