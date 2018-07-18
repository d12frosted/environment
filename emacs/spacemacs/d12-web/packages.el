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
    (company-restclient :requires company)
    restclient
    org-web-tools)
  "The list of Lisp packages required by the d12-web layer.")

(defun d12-web/init-restclient ()
  (use-package restclient
    :defer t
    :init
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))))

(defun d12-web/init-company-restclient ()
  (use-package company-restclient
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-restclient
            :modes restclient-mode)))

(defun d12-web/init-eww ())

(defun d12-web/post-init-org-web-tools ()
  (use-package org-web-tools
    :defer t))

;;; packages.el ends here
