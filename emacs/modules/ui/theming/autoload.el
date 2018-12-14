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
     ;; modeline
     '(doom-modeline-buffer-modified ((t (:inherit (mode-line bold) :foreground "#F4D7DA"))) t)
     '(doom-modeline-info ((t (:inherit mode-line))) t)
     '(doom-modeline-persp-name ((t (:inherit mode-line :foreground "SkyBlue3"))) t)
     '(doom-modeline-buffer-project ((t (:inherit mode-line :foreground "SkyBlue3"))) t)
     '(doom-modeline-project-root-dir ((t (:inherit mode-line))) t)
     '(doom-modeline-persp-buffer-not-in-persp ((t (:inherit mode-line))) t)

     ;; org
     '(org-checkbox ((t (:background "#FAF7CC"))) t)

     ;; solaire
     '(solaire-default-face ((t (:inherit default :background "#FAFAFA")))))))
