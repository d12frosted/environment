;;; packages.el --- d12-csharp layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst d12-csharp-packages
  '(
    csharp-mode
    omnisharp
    )
  "The list of Lisp packages required by the d12-csharp layer.")

(defun d12-csharp/post-init-csharp-mode ()
  (use-package csharp-mode
    :defer t
    :config
    (defun d12-csharp/go-to-definition-at-center ()
      (interactive)
      (progn
        (omnisharp-go-to-definition)
        (recenter)))
    (bind-keys
     :map csharp-mode-map
     ("M-." . d12-csharp/go-to-definition-at-center)
     ("M-," . pop-tag-mark))))

(defun d12-csharp/post-init-omnisharp ()
  (use-package omnisharp
    :defer t
    :init
    (setq-default omnisharp-server-executable-path (executable-find "omnisharp"))))

;;; packages.el ends here
