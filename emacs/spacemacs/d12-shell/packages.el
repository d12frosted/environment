;;; packages.el --- d12-shell layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst d12-shell-packages
  '(
    multi-term
    )
  "The list of Lisp packages required by the d12-shell layer.")

(defun d12-shell/post-init-multi-term ()
  (use-package multi-term
    :config
    (setq multi-term-program (executable-find "fish"))))

;;; packages.el ends here
