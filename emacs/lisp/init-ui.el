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

(defconst ui-font-family-monospaced "Source Code Pro")
(defconst ui-font-family-proportional "Source Code Pro")
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



(use-package svg-lib
  :defer t
  :init
  (setq-default
   svg-lib-icons-dir (expand-file-name "svg-lib/" path-etc-dir))
  (setq-default
   svg-lib-icon-collections
   (list
    (cons "bootstrap" "https://icons.getbootstrap.com/assets/icons/%s.svg")
    (cons "simple" "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
    (cons "octicons" "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
    (cons "fa-solid" (concat
                      "file://"
                      (expand-file-name
                       "~/Dropbox/resources/icons/fontawesome/svgs/solid/%s.svg"))))))

(use-package svg-tag-mode
  :straight (:type git :host github :repo "rougier/svg-tag-mode")
  :commands (svg-tag-mode))

(use-package all-the-icons
  :defer t
  :commands (all-the-icons-material)
  :init
  (when elpa-bootstrap-p
    (let ((file (expand-file-name "all-the-icons-ready"
                                  path-cache-dir)))
      (unless (file-exists-p file)
        (all-the-icons-install-fonts t)
        (shell-command-to-string (format "echo '' > %s" file))))))

(use-package nano
  :straight (:type git :host github :repo "d12frosted/nano-emacs" :branch "fix-non-gui-usage")
  :defer t
  :init
  (setq nano-font-family-monospaced ui-font-family-monospaced)
  (setq nano-font-family-proportional ui-font-family-proportional)
  (setq nano-font-size ui-font-size)

  (require 'nano-layout)
  (require 'nano-theme-light)

  (require 'nano-colors)
  (require 'nano-faces)

  (nano-faces)
  (require 'nano-theme)
  (nano-theme--basics)
  (nano-theme--font-lock)
  (nano-theme--mode-line)
  (nano-theme--minibuffer)
  (nano-theme--buttons)
  (nano-theme--info)
  (nano-theme--bookmark)
  (nano-theme--speedbar)
  (nano-theme--message)
  (nano-theme--outline)
  (nano-theme--customize)
  (nano-theme--package)
  (nano-theme--flyspell)
  (nano-theme--ido)
  (nano-theme--diff)
  (nano-theme--term)
  (nano-theme--calendar)
  (nano-theme--agenda)
  (nano-theme--org)
  (nano-theme--mu4e)
  (nano-theme--elfeed)
  (nano-theme--rst)
  (nano-theme--markdown)
  (nano-theme--hl-line)
  (nano-theme--company)

  ;; (require 'nano-defaults)
  (require 'nano-modeline)
  (require 'nano-help)

  ;; custom faces
  (with-eval-after-load 'org
    (set-face 'org-done 'nano-face-faded)
    (set-face 'org-verbatim 'nano-face-strong)
    (set-face-attribute 'org-level-1 nil
                        :overline nano-color-subtle
                        :family ui-font-family-proportional
                        :height (* 10 (floor (* 1.2 ui-font-size))))
    (set-face-attribute 'org-level-2 nil
                        :overline nano-color-subtle
                        :family ui-font-family-proportional
                        :height (* 10 (floor (* 1.1 ui-font-size))))
    (set-face-attribute 'org-level-3 nil
                        :family ui-font-family-proportional
                        :height (* 10 (floor (* 1.1 ui-font-size))))
    (set-face-attribute 'org-document-info nil
                        :inherit 'nano-face-subtle)
    (set-face-attribute 'org-document-title nil
                        :foreground nano-color-foreground
                        :family ui-font-family-proportional
                        :height (* 10 (floor (* 1.1 ui-font-size)))
                        :weight 'medium))
  (with-eval-after-load 'markdown-mode
    (set-face 'markdown-inline-code-face 'nano-face-strong)
    (set-face 'markdown-pre-face 'hl-line)))

(use-package nano-modeline
  :straight (:type git :host github :repo "rougier/nano-modeline"))

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
