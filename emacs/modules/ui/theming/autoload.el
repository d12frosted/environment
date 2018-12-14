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
     ;; theming faces
     '(+theming-mode-line ((t (:inherit mode-line))))
     '(+theming-mode-line-highlight ((t (:inherit mode-line :foreground "SkyBlue3"))))
     '(+theming-mode-line-warning ((t (:inherit warning))))
     '(+theming-mode-line-critical ((t (:inherit warning :foreground "#F4847D"))))

     ;; org faces
     '(org-checkbox ((t (:background "#FAF7CC"))))

     ;; solaire faces
     '(solaire-default-face ((t (:inherit default :background "#FAFAFA")))))))
