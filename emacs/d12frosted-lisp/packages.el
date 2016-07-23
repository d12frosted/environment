;;; packages.el --- d12frosted-lisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-lisp-packages
  '(emacs-lisp
    lispy))

(defun d12frosted-lisp/post-init-emacs-lisp ()
  ;; (setq lisp-indent-function 'common-lisp-indent-function)
  )

(defun d12frosted-lisp/init-lispy ()
  (use-package lispy
      :commands (lispy-mode)
      :init
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      :config
      ;; unbind some key bindings
      (unbind-key "M-RET" lispy-mode-map)
      (bind-key "C-a" 'mwim-beginning-of-code-or-line lispy-mode-map)))

;;; packages.el ends here
