;;; init-window.el --- taming them windows -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
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

(require 'init-keybindings)

;; Prefer vertical splits by default
(setq split-height-threshold nil
      split-width-threshold 160)

(use-package ace-window
  :defer t
  :init
  (setq aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o)))

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

(provide 'init-window)
;;; init-window.el ends here
