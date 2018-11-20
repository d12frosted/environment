;;; emacs/ui/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

(setq-default
 ;; kill compilation process before starting another
 compilation-always-kill t

 ;; save all buffers on `compile'
 compilation-ask-about-save nil

 ;; please ask me before creating non-existing stuff
 confirm-nonexistent-file-or-buffer t

 ;; avoid resizing during font changes
 frame-inhibit-implied-resize t

 ;; always avoid GUI
 use-dialog-box nil

 ;; no beeping and no blinking please
 ring-bell-function #'ignore
 visible-bell nil

 ;; do not highlight regions in non-selected windows
 highlight-nonselected-windows nil

 ;; make sure that trash is not drawed
 indicate-buffer-boundaries nil
 indicate-empty-lines nil

 ;; don't resize emacs in steps, it looks weird and plays bad with
 ;; window manager.
 window-resize-pixelwise t
 frame-resize-pixelwise t

 ;; disable bidirectional text for tiny performance boost
 bidi-display-reordering nil)

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; Truly silence startup message
(fset #'display-startup-echo-area-message #'ignore)

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
