;;; lisp/init-window.el -*- lexical-binding: t; -*-
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

(use-package ace-window
  :general
  (+leader-def
    "C-w" '(ace-window :which-key "Ace window")
    "M-w" '(ace-window :which-key "Ace window"))
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
  "Close other windows to focus on this one. Activate again to
undo this. If the window changes before then, the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(+leader-def
  "w"  '(nil :which-key "window...")
  "wV" '(+window-split-vertically :which-key "Split frame vertically")
  "wv" '(+window-split-vertically-and-focus :which-key "Split frame vertically and focus")
  "wS" '(+window-split-horizontally :which-key "Split frame horizontally")
  "ws" '(+window-split-horizontally-and-focus :which-key "Split frame horizontally and focus")
  "wk" '(delete-window :which-key "Kill window")
  "wm" '(+window-zoom :which-key "Kill other windows"))

(provide 'init-window)
;;; init-window.el ends here
