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

     ;; git-gutter faces
     '(git-gutter:added ((t (:foreground "#97f295"))))
     '(git-gutter:modified ((t (:inherit warning))))
     '(git-gutter:deleted ((t (:foreground "#F4847D"))))

     ;; dired-k
     '(dired-k-added ((t (:foreground "#97f295"))))
     '(dired-k-modified ((t (:inherit warning))))
     '(dired-k-untracked ((t (:inherit error))))
     '(dired-k-commited ((t (:foreground "#ffffff"))))
     '(dired-k-ignored ((t (:foreground "DimGrey"))))

     ;; org faces
     '(org-mode-line-clock ((t (:inherit +theming-mode-line-warning
                                :box unspecified
                                :foreground unspecified
                                :background unspecified))))
     '(org-checkbox ((t (:background "#FAF7CC"))))

     ;; solaire faces
     '(solaire-default-face ((t (:inherit default :background "#FAFAFA")))))))
