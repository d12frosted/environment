;;; ui/theming/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 25 Nov 2018
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

;;;###autoload
(defun +leuven|patch-faces ()
  "Patch faces of `leuven-theme'."
  (when (eq +modern-theme 'leuven)
    (custom-set-faces
     '(+modeline-info ((t (:foreground "PaleGreen1"))) t))))
