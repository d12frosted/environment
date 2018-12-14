;;; ui/modeline/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Nov 2018
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

(def-package! doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init)
  :init
  (custom-set-faces
   '(doom-modeline-buffer-modified ((t (:inherit (+theming-mode-line-critical bold)))))
   '(doom-modeline-info ((t (:inherit +theming-mode-line))))
   '(doom-modeline-warning ((t (:inherit +theming-mode-line-warning))))
   '(doom-modeline-urgent ((t (:inherit +theming-mode-line-critical))))
   '(doom-modeline-buffer-project ((t (:inherit +theming-mode-line-highlight))))
   '(doom-modeline-project-root-dir ((t (:inherit +theming-mode-line-highlight))))
   '(doom-modeline-persp-name ((t (:inherit +theming-mode-line-highlight))))
   '(doom-modeline-persp-buffer-not-in-persp ((t (:inherit +theming-mode-line-warning)))))
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project))
