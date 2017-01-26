;;; packages.el --- d12frosted-csharp layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-csharp-packages
  '(omnisharp
    csharp-mode
    smart-ops))

(defun d12frosted-csharp/post-init-csharp-mode ()
  (use-package csharp-mode
    :init
    (add-hook 'csharp-mode-hook 'flycheck-mode)
    :config
    (bind-keys
     :map csharp-mode-map
     ("M-." . d12-csharp/go-to-definition-at-center)
     ("M-," . pop-tag-mark))))

(defun d12frosted-csharp/post-init-smart-ops ()
  (use-package smart-ops
    :config
    (define-smart-ops-for-mode 'csharp-mode
      (smart-ops "," :pad-before nil)
      (smart-ops "<" :pad-before nil :pad-after nil)
      (smart-ops ">" :pad-before nil :pad-after nil)
      (smart-ops-default-ops))))


;;; packages.el ends here
