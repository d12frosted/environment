;;; ui/helpful/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 25 Jun 2019
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun +helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))
