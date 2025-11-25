;;; init-ui.el --- UI configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
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
;; Visual appearance: theme (modus), fonts (fontaine), spacing, mode-line
;; (minions), SVG icons, and various display settings. Frame customizations
;; are in early-init.el.
;;
;;; Code:

(require 'init-env)

;; no startup  screen
(setq inhibit-startup-screen t)

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; initial buffer
(setq initial-buffer-choice nil)

;; no frame title
(setq frame-title-format nil)

;; no file dialog
(setq use-file-dialog nil)

;; no dialog box
(setq use-dialog-box nil)

;; no empty line indicators
(setq indicate-empty-lines nil)

;; no cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

(setq initial-scratch-message nil)
(setq inhibit-default-init t)

;; start easy with little dependencies to load
(setq initial-major-mode 'fundamental-mode)

;; yet keep `text-mode' as default major mode
(setq default-major-mode 'text-mode)

;; maximum font lock
(setq font-lock-maximum-decoration t)

;; no confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(basic substring))

;; disable cursort blinking, cause it drives me nuts
(blink-cursor-mode -1)

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; no beeping and no blinking please
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

;; make sure that trash is not drawn
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

;; don't resize emacs in steps, it looks weird and plays bad with
;; window manager.
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; disable bidirectional text for tiny performance boost
(setq bidi-display-reordering nil)

(use-package minions
  :ensure t
  :init
  (minions-mode 1))

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Vertical window divider
(setq window-divider-default-right-width 18)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; use suitable font for emojis on macos
(when env-sys-mac-p
  (set-fontset-font
   t 'emoji
   '("Apple Color Emoji" . "iso10646-1") nil 'prepend))



(use-package font-lock+
  :disabled t
  :ensure (font-lock+ :host github :repo "emacsmirror/font-lock-plus" :wait t)
  :demand t)



(use-package widget-extra
  :ensure (widget-extra :host github :repo "d12frosted/widget-extra" :wait t)
  :demand t)



(use-package svg-lib
  :ensure (:host github :repo "rougier/svg-lib")
  :defer t
  :init
  (setq-default
   svg-lib-icons-dir (expand-file-name "svg-lib/" path-etc-dir))
  (setq-default
   svg-lib-icon-collections
   (list
    (cons "custom" (concat
                    "file://"
                    (expand-file-name
                     "resources/icons/custom/%s.svg"
                     path-cloud-dir)))
    (cons "bootstrap" (concat
                       "file://"
                       (expand-file-name
                        "icons/bootstrap/%s.svg"
                        path-cache-dir)))
    (cons "fa-brands" (concat
                       "file://"
                       (expand-file-name
                        "icons/fontawesome/svgs/brands/%s.svg"
                        path-cache-dir)))
    (cons "fa-regular" (concat
                        "file://"
                        (expand-file-name
                         "icons/fontawesome/svgs/regular/%s.svg"
                         path-cache-dir)))
    (cons "fa-solid" (concat
                      "file://"
                      (expand-file-name
                       "icons/fontawesome/svgs/solid/%s.svg"
                       path-cache-dir))))))

(use-package svg-tag-mode
  :ensure (:host github :repo "rougier/svg-tag-mode")
  :commands (svg-tag-mode
             svg-tag-mode-on)
  :init
  (advice-add #'svg-tag-mode-on :around #'fun-silent))

;; when theme is right, this thing is good
(global-hl-line-mode)

;; no clutter, please!
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; but menu bar is ok on macos
(if (and env-graphic-p env-sys-mac-p)
    (menu-bar-mode t)
  (menu-bar-mode -1))



(use-package colorful-mode
  :ensure t
  :defer t)



(use-package fancy-compilation
  :ensure (:host codeberg :repo "ideasman42/emacs-fancy-compilation")
  :commands (fancy-compilation-mode)
  :init
  (setf fancy-compilation-override-colors nil)
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))



(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-common-palette-overrides
        (append
         '((overline-heading-1 bg-blue-subtle)
           (overline-heading-2 bg-magenta-nuanced))
         modus-themes-preset-overrides-cooler)
        modus-themes-headings
        '((1 . (variable-pitch heavy 2.0))
          (2 . (variable-pitch bold 1.6))
          (3 . (variable-pitch semibold 1.3))
          (4 . (variable-pitch semibold 1.1))
          (5 . (variable-pitch medium 1.0))
          (t . (variable-pitch medium 1.0))))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package fontaine
  :ensure t
  :if env-graphic-p
  :config
  (setq fontaine-presets
        '((regular
           :default-height 140)
          (medium
           :default-weight semilight
           :default-height 140)
          (large
           :default-weight semilight
           :default-height 180
           :bold-weight extrabold)
          (t ; our shared fallback properties
           :default-family "Iosevka Term Curly"
           :default-weight normal
           ;; :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Aile"
           :variable-pitch-weight normal
           :variable-pitch-height 1.0
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing 0.2)))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 14
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 14
          :scroll-bar-width 8))
  (spacious-padding-mode 1))



(provide 'init-ui)
;;; init-ui.el ends here
