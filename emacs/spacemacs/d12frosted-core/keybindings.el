;;; keybindings.el --- d12frosted-core layer keybindings file for Spacemacs.
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

(bind-keys
 ("M-g M-g"         . d12/goto-line-and-center)
 ("C-w"             . d12/kill-line-or-region)
 ("M-w"             . d12/copy-line-or-region)
 ("C-S-<backspace>" . d12/delete-line-or-region)
 ("<f9>"            . calc)
 ("<f8>"            . calendar))

(evil-leader/set-key
  "p#" 'projectile-replace-regexp
  "of" 'd12-interesting-files
  "os" 'd12/sync-spacemacs)

(spacemacs/declare-prefix "o" "d12frosted")

;; The worst key binding ever! If I ever want to quit Emacs, I'd call my doctor.
(unbind-key "C-x C-c")

;;; keybindings.el ends here
