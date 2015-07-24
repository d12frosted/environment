;;; keybindings.el --- d12frosted Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Boris Buliga & Contributors
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bind-keys
 ("M-g g"           . d12/goto-line-and-center)
 ("C-w"             . d12/cut-line-or-region)
 ("M-w"             . d12/copy-line-or-region)
 ("C-S-<backspace>" . d12/delete-line-or-region)
 ("C-c u d"         . d12/duplicate-line-or-region)
 ("M-;"             . comment-dwim-2)
 )
