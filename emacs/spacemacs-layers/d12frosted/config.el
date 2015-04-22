;;; config.el --- d12frosted Layer config File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Declare prefixes

(spacemacs/declare-prefix "o" "d12frosted")
(spacemacs/declare-prefix "oi" "insert")

;;; js
;; configurations specifc to js

(setq js-indent-level 2)

;;; text-mode
;; configurations specific to text-mode and all modes derived from text-mode

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

;;; random stuff

(add-hook 'before-save-hook 'delete-trailing-whitespace)
