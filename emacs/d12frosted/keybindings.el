;;; keybindings.el --- d12frosted Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bind-keys
 ("M-g g"           . d12/goto-line-and-center)
 ("C-w"             . d12/kill-line-or-region)
 ("M-w"             . d12/copy-line-or-region)
 ("C-S-<backspace>" . d12/delete-line-or-region)
 ("M-;"             . comment-dwim-2)
 ("M-C-;"           . comment-or-uncomment-sexp))

(evil-leader/set-key
  "og" 'd12/helm-gtd
  "oc" 'd12/helm-configs)

(spacemacs/declare-prefix "o" "d12frosted")
