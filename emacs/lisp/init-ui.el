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
;; Making Emacs beautiful. See also `early-init'.
;;
;;; Code:

(require 'init-env)

(defconst ui-font-family-mono "Source Code Pro")
(defconst ui-font-family-sans "Source Sans Pro")
(defconst ui-font-family-serif "Source Serif Pro")
(defconst ui-font-size (if env-sys-mac-p 14 12))

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
  :init
  (minions-mode 1))

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)



(use-package font-lock+)



(use-package bui
  :defer t)

(use-package lister
  :straight (lister
             :type git
             :host github
             :repo "publicimageltd/lister"
             :branch "archive-version-0.7.2")
  :defer t)



;; When we set a face, we take care of removing any previous settings
(defun ui-set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))

(use-package nano-theme
  :straight (:type git :host github :repo "rougier/nano-theme")
  :config
  (set-face-attribute 'nano-mono nil
                      :family ui-font-family-mono
                      :height (* 10 ui-font-size))
  (set-face-attribute 'nano-sans nil
                      :family ui-font-family-sans
                      :height (* 10 ui-font-size))
  (set-face-attribute 'nano-serif nil
                      :family ui-font-family-serif
                      :height (* 10 ui-font-size))

  (setq nano-fonts-use t)

  ;; Vertical window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)

  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?… 'nano-faded))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?- 'nano-faded))

  ;; Nerd font for glyph icons
  (let ((roboto-nerd (font-spec :name "RobotoMono Nerd Font Mono")))
    (if (and (find-font roboto-nerd)
             (fboundp 'set-fontset-font))
        (set-fontset-font t '(#xe000 . #xffdd) roboto-nerd)
      (message "Roboto Mono Nerd font has not been found on your system")))

  (load-theme 'nano-light t)

  ;; custom faces
  (with-eval-after-load 'org
    ;; (set-face 'org-done 'nano-face-faded)
    ;; (set-face-underline 'org-verbatim nil)
    (set-face-attribute 'org-level-1 nil
                        :overline nano-light-subtle
                        :family ui-font-family-mono
                        :height (* 10 (floor (* 1.2 ui-font-size))))
    (set-face-attribute 'org-level-2 nil
                        :overline nano-light-subtle
                        :family ui-font-family-mono
                        :height (* 10 (floor (* 1.1 ui-font-size))))
    (set-face-attribute 'org-level-3 nil
                        :family ui-font-family-mono
                        :height (* 10 (floor (* 1.1 ui-font-size))))
    (set-face-attribute 'org-document-title nil
                        :foreground nano-light-foreground
                        :family ui-font-family-mono
                        :height (* 10 (floor (* 1.2 ui-font-size)))
                        :weight 'medium)
    (ui-set-face 'org-date-selected 'nano-popout-i))
  (with-eval-after-load 'markdown-mode
    (ui-set-face 'markdown-pre-face 'hl-line))
  (with-eval-after-load 'magit
    (set-face-attribute 'magit-diff-lines-heading nil
                        :background (face-background 'nano-popout-i))
    (set-face-attribute 'magit-diff-lines-boundary nil
                        :background (face-background 'nano-popout-i))))

(use-package svg-lib
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
                        "~/Dropbox/resources/icons/custom/%s.svg")))
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
  :straight (:type git :host github :repo "rougier/svg-tag-mode")
  :commands (svg-tag-mode
             svg-tag-mode-on))

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



(provide 'init-ui)
;;; init-ui.el ends here
