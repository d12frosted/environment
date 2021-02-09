;;; init-ui.el --- UI configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Making Emacs beautiful.
;;
;;; Code:

(require 'init-env)

;; be quiet at startup; don't load or display anything unnecessary
(setq-default
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 use-file-dialog nil
 use-dialog-box nil)

;; disable cursort blinking
(blink-cursor-mode -1)

;; Less clutter
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
;; for some reason only this removes the clutter with xmonad
(when env-graphic-p
  (scroll-bar-mode -1))

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; mode line
(column-number-mode)
(size-indication-mode)

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

 ;; hide curosrs in other windoes
 cursor-in-non-selected-windows nil)

(when (and env-sys-mac-p env-graphic-p)
  (setq-default line-spacing 1)
  (add-to-list 'default-frame-alist '(font . "Source Code Pro")))

(use-package modus-themes
  :if env-graphic-p
  :init
  (setq-default
   modus-themes-diffs 'desaturated
   modus-themes-headings '((t . section))
   modus-themes-bold-constructs t
   modus-themes-syntax 'faint
   modus-themes-prompts 'subtle
   modus-themes-completions 'opionated)
  (load-theme 'modus-operandi t))

(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :init
  (setq-default flycheck-color-mode-line-show-running nil))

(use-package all-the-icons
  :defer t
  :commands (all-the-icons-material))

(use-package minions
  :commands (minions-mode)
  :init (minions-mode 1))

(use-package mode-line-debug
  :commands (mode-line-debug-mode)
  :init (mode-line-debug-mode 1))

;; Install it from sources, because ELPA version has invalid
;; signature.
(use-package spinner
  :quelpa (spinner
           :fetcher github
           :repo "Malabarba/spinner.el"))

(provide 'init-ui)
;;; init-ui.el ends here
