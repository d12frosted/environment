;;; packages.el --- d12-git layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst d12-git-packages
  '(ghub
    )
  "The list of Lisp packages required by the d12-git layer.")

(defun d12-git/init-ghub ()
  (use-package ghub
    :defer t))

;;; packages.el ends here
