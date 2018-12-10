;;; ui.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;; Most of the code was borrowed from hlissner/doom-emacs.
;;
;;; Commentary:
;;
;;; Code:

;;
;; Advice

;;;###autoload
(defun nucleus*shut-up (orig-fn &rest args)
  "Generic advisor for silencing noisy functions."
  (quiet! (apply orig-fn args)))

;;
;; Commands

;;;###autoload
(defun nucleus/delete-frame ()
  "Delete the current frame, but ask for confirmation if it isn't
empty."
  (interactive)
  (if (cdr (frame-list))
      (when (nucleus-quit-p "Close frame?")
        (delete-frame))
    (save-buffers-kill-emacs)))

;;;###autoload
(defun nucleus/window-zoom ()
  "Close other windows to focus on this one. Activate again to
undo this. If the window changes before then, the undo expires.

Alternatively, use `nucleus/window-enlargen'."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(defvar nucleus--window-enlargened nil)
;;;###autoload
(defun nucleus/window-enlargen ()
  "Enlargen the current window to focus on this one. Does not
close other windows (unlike `nucleus/window-zoom') Activate again
to undo."
  (interactive)
  (setq nucleus--window-enlargened
        (if (and nucleus--window-enlargened
                 (assq ?_ register-alist))
            (ignore (ignore-errors (jump-to-register ?_)))
          (window-configuration-to-register ?_)
          (if (window-dedicated-p)
              ;; `window-resize' and `window-max-delta' don't respect
              ;; `ignore-window-parameters', so we gotta force it to.
              (cl-letf* ((old-window-resize (symbol-function #'window-resize))
                         (old-window-max-delta (symbol-function #'window-max-delta))
                         ((symbol-function #'window-resize)
                          (lambda (window delta &optional horizontal _ignore pixelwise)
                            (funcall old-window-resize window delta horizontal
                                     t pixelwise)))
                         ((symbol-function #'window-max-delta)
                          (lambda (&optional window horizontal _ignore trail noup nodown pixelwise)
                            (funcall old-window-max-delta window horizontal t
                                     trail noup nodown pixelwise))))
                (maximize-window))
            (maximize-window))
          t)))

;;;###autoload
(defun nucleus/set-frame-opacity (opacity)
  "Interactively change the current frame's opacity.

OPACITY is an integer between 0 to 100, inclusive."
  (interactive
   (list (read-number "Opacity (0-100): "
                      (or (frame-parameter nil 'alpha)
                          100))))
  (set-frame-parameter nil 'alpha opacity))
