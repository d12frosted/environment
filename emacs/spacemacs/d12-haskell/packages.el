;;; packages.el --- d12-haskell layer packages file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2017 Boris Buliga
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

(defconst d12-haskell-packages
  '(intero
    haskell-mode)
  "The list of Lisp packages required by the d12-haskell layer.")

(defun d12-haskell/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :init
    ;; This block executes before the package has been loaded
    :config
    ;; This block executes after the package has been loaded
    ))

(defun d12-haskell/init-intero ()
  (use-package intero
    :defer t
    :init
    (add-hook 'haskell-mode-hook 'intero-mode)
    :config))


;;; packages.el ends here
