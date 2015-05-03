;;; syntax-checking-funcs.el --- funcs file of syntax checking configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(defun syntax-checking/post-init-popwin ()
  (push '("^\*Flycheck.+\*$" :regexp t :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config))
