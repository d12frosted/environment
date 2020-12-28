;;; init-ui.el --- all the nice things -*- lexical-binding: t; -*-
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

(require 'use-package)
(require 'init-env)

;; disable cursort blinking
(blink-cursor-mode -1)

;; Less clutter
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(scroll-bar-mode -1) ; for some reason only this removes the clutter with xmonad

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

(use-package modus-themes
  :if +sys-graphic-p
  :init
  (setq modus-themes-diffs 'desaturated
        modus-themes-headings '((t . section))
        modus-themes-bold-constructs t
        modus-themes-syntax 'faint
        modus-themes-prompts 'subtle
        modus-themes-completions 'opionated)
  (load-theme 'modus-operandi t))

(use-package leuven-theme
  :if +sys-graphic-p
  :disabled
  :init
  (load-theme 'leuven t)
  (custom-set-faces
   ;; theming faces
   '(+theming-mode-line ((t (:inherit mode-line))))
   '(+theming-mode-line-highlight ((t (:inherit mode-line :foreground "SkyBlue3"))))
   '(+theming-mode-line-warning ((t (:inherit warning))))
   '(+theming-mode-line-critical ((t (:inherit warning :foreground "#F4847D"))))

   ;; flycheck
   '(flycheck-color-mode-line-error-face ((t :inherit mode-line
                                             :foreground "LavenderBlush"
                                             :background "DarkSalmon")))
   '(flycheck-color-mode-line-warning-face ((t :inherit mode-line
                                               :foreground "LavenderBlush4"
                                               :background "khaki")))
   '(flycheck-color-mode-line-info-face ((t :inherit mode-line
                                            :foreground "black"
                                            :background "LightSkyBlue")))

   ;; git-gutter faces
   '(git-gutter:added ((t (:foreground "#97f295"))))
   '(git-gutter:modified ((t (:inherit warning))))
   '(git-gutter:deleted ((t (:foreground "#F4847D"))))

   ;; dired-k
   '(dired-k-added ((t (:foreground "#97f295"))))
   '(dired-k-modified ((t (:inherit warning))))
   '(dired-k-untracked ((t (:inherit error))))
   '(dired-k-commited ((t (:foreground "#ffffff"))))
   '(dired-k-ignored ((t (:foreground "DimGrey"))))

   ;; org faces
   '(org-mode-line-clock ((t (:inherit +theming-mode-line-warning
                                       :box unspecified
                                       :foreground unspecified
                                       :background unspecified))))
   '(org-checkbox ((t (:background "#FAF7CC"))))
   '(org-agenda-dimmed-todo-face ((((background light)) (:foreground "grey50"))
                                  (((background dark)) (:foreground "grey50"))))
   '(org-roam-link ((t (:inherit org-link :foreground "#8359A3"))))
   '(org-roam-link-current ((t (:inherit org-link :foreground "#F4847D"))))

   ;; solaire faces
   '(solaire-default-face ((t (:inherit default :background "#FAFAFA"))))

   ;; lsp-ui faces
   '(lsp-ui-sideline-code-action ((t (:inherit default :foreground "SkyBlue3"))))
   '(lsp-ui-doc-background ((((background light)) :background "#FAF7CC")
                            (t :background "#272A36")))
   ))

(use-package flycheck-color-mode-line
  :after flycheck
  :init
  (setq flycheck-color-mode-line-show-running nil)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(when (and +sys-mac-p +sys-graphic-p)
  (setq-default line-spacing 1)
  (add-to-list 'default-frame-alist '(font . "Source Code Pro")))

(use-package all-the-icons
  :defer t
  :commands (all-the-icons-material))

;; (use-package unicode-fonts
;;   :init
;;   (unicode-fonts-setup))

(use-package minions
  :commands (minions-mode)
  :init (minions-mode 1))

(use-package mode-line-debug
  :commands (mode-line-debug-mode)
  :init (mode-line-debug-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
