;;; packages.el --- d12-web layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst d12-web-packages
  '((eww :location built-in)
    org-web-tools)
  "The list of Lisp packages required by the d12-web layer.")

(defun d12-web/init-eww ())

(defun d12-web/post-init-org-web-tools ()
  (use-package org-web-tools))

;;; packages.el ends here
