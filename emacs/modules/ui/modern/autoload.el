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
(defun +modern-font-size ()
  "Return target font pixel size for current display."
  (when window-system (if (> (x-display-pixel-width) 1920) 18 12)))

;;;###autoload
(defun +modern-big-font-size ()
  "Return target big font pixel size for current display."
  (when window-system (if (> (x-display-pixel-width) 1920) 28 18)))

;;;###autoload
(defun +modern-dpi (&optional display)
  "Get the DPI of DISPLAY.

DISPLAY is a display name, frame or terminal, as in
`display-monitor-attributes-list'."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

;;;###autoload
(defun +modern-patch-font-size (font-spec size)
  "Patch size of FONT-SPEC according to display DPI."
  (font-put font-spec :size (round (* (+modern-scale) size)))
  font-spec)

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
