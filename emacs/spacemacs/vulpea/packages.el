;;; packages.el --- vulpea layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst vulpea-packages
  '((vulpea :location built-in))
  "The list of Lisp packages required by the vulpea layer.")

(defun vulpea/init-vulpea ()
  (add-to-load-path-if-exists (concat d12-path-spacemacs-config-home "vulpea/local/vulpea/"))
  (use-package vulpea
    :defer t
    :init
    (spacemacs|use-package-add-hook org
      :post-config
      (require 'vulpea))
    :config))

;;; packages.el ends here
