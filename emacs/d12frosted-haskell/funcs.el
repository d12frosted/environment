;;; funcs.el --- d12frosted-haskell layer funcs file for Spacemacs.
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

(defun d12frosted-haskell/set-indentation-step ()
  (setq tab-width 2))

(defun d12frosted-haskell/maybe-haskell-interactive-mode ()
  (unless (bound-and-true-p org-src-mode)
    (interactive-haskell-mode)))

(defun d12frosted-haskell/setup-package-yaml-save ()
  (interactive)
  (add-hook 'after-save-hook #'d12frosted-haskell/on-package-yaml-save nil t))

(defun d12frosted-haskell/on-package-yaml-save ()
  (interactive)
  (when (and (string-equal (file-name-nondirectory buffer-file-name) "package.yaml")
             (file-exists-p (concat (file-name-directory buffer-file-name)
                                    "stack.yaml")))
    (call-process-shell-command (executable-find "hpack"))))

;;; funcs.el ends here
