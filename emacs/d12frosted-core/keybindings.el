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
 ("M-g M-g"         . d12/goto-line-and-center)
 ("C-w"             . d12/kill-line-or-region)
 ("M-w"             . d12/copy-line-or-region)
 ("C-S-<backspace>" . d12/delete-line-or-region)
 ("M-C-;"           . d12/comment-or-uncomment-sexp)
 ("<f9>"            . calc)
 ("<f8>"            . calendar))

(evil-leader/set-key
  "p#" 'projectile-replace-regexp
  "oj" 'd12-interesting-files)

(spacemacs/declare-prefix "o" "d12frosted")

;;; keybindings.el ends here
