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

;;; funcs.el ends here
