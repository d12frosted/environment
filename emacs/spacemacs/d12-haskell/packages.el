;;; packages.el --- d12-haskell layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst d12-haskell-packages
  '(haskell-mode)
  "The list of Lisp packages required by the d12-haskell layer.")

(defun d12-haskell/post-init-haskell-mode ()
  (setq haskell-hoogle-command "hoogle -i"))

;;; packages.el ends here
