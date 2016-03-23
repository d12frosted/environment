;;; keybindings.el --- d12frosted-core layer keybindings file for Spacemacs.
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

(bind-keys
 ("M-g g"           . d12/goto-line-and-center)
 ("C-w"             . d12/kill-line-or-region)
 ("M-w"             . d12/copy-line-or-region)
 ("C-S-<backspace>" . d12/delete-line-or-region)
 ("M-C-;"           . comment-or-uncomment-sexp))

(evil-leader/set-key
  "oj" 'd12-helm
  "p#" 'projectile-replace-regexp)

(spacemacs/declare-prefix "o" "d12frosted")
;; (spacemacs/declare-prefix "." "org")

;;; keybindings.el ends here
