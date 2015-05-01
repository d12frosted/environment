;;; packages.el --- mu4e Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Boris Buliga
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar mu4e-packages
  '(mu4e-maildirs-extension)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar mu4e-excluded-packages '()
  "List of packages to exclude.")

(defun mu4e/init-mu4e-maildirs-extension ()
  "Initialize 'mu4e-maildirs-extension package"
  (use-package mu4e-maildirs-extension
    :defer 1
    :init
    :config
    (mu4e-maildirs-extension)
    (add-hook 'mu4e-index-updated-hook 'mu4e-maildirs-extension-index-updated-handler)
    (add-hook 'mu4e-main-mode-hook 'mu4e-maildirs-extension-index-updated-handler)
    (setq mu4e-maildirs-extension-maildir-separator "*"
          mu4e-maildirs-extension-submaildir-separator "»")))
