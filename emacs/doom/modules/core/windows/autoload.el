;;; core/windows/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +window-split-vertically ()
  "Split window vertically."
  (interactive)
  (split-window-right))

;;;###autoload
(defun +window-split-vertically-and-focus ()
  "Split window vertically and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

;;;###autoload
(defun +window-split-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-below))

;;;###autoload
(defun +window-split-horizontally-and-focus ()
  "Split window horizontally and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

;;;###autoload
(defun +window-zoom ()
  "Close other windows to focus on this one.

Activate again to undo this. If the window changes before then,
the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))
