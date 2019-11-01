;;; lisp/init-ui.el -*- lexical-binding: t; -*-
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

;; disable cursort blinking
(blink-cursor-mode -1)

;; Less clutter
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(scroll-bar-mode -1) ; for some reason only this removes the clutter with xmonad

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

(setq-default
 ;; no beeping and no blinking please
 ring-bell-function #'ignore
 visible-bell nil

 ;; make sure that trash is not drawed
 indicate-buffer-boundaries nil
 indicate-empty-lines nil

 ;; don't resize emacs in steps, it looks weird and plays bad with
 ;; window manager.
 window-resize-pixelwise t
 frame-resize-pixelwise t

 ;; disable bidirectional text for tiny performance boost
 bidi-display-reordering nil

 ;; do not highlight regions in non-selected windows
 highlight-nonselected-windows nil

 ;; hide curosrs in other windoes
 cursor-in-non-selected-windows nil
)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(use-package leuven-theme
  :init
  (load-theme 'leuven t))

(provide 'init-ui)
;;; init-ui.el ends here
