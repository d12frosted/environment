;;; ui/modern/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
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

;;;###autoload
(defun +modern/reload-font ()
  "Reload `+modern-font', `+modern-variable-pitch-font', and
`+modern-unicode-font', if set."
  (interactive)
  (when +modern-font
    (set-frame-font +modern-font t))
  (+modern|init-fonts))

;;;###autoload
(defun +modern/reload-theme ()
  "Reset the current color theme and fonts."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) +modern-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (+modern|init-theme)
    (+modern|init-fonts)))

;;;###autoload
(defun +modern|apply-ansi-color-to-compilation-buffer ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))

;;;###autoload
(define-minor-mode +modern-big-font-mode
  "A global mode that resizes the font, for streams,
screen-sharing and presentations.

Uses `+modern-big-font' when enabled."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless +modern-big-font
    (user-error "`+modern-big-font' must be set to a valid font"))
  (unless +modern-font
    (user-error "`+modern-font' must be set to a valid font"))
  (set-frame-font (if +modern-big-font-mode
                      +modern-big-font
                    +modern-font)
                  t t))
