;;; core-configs.el --- configs file of core configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Debug
;; =======

(setq message-log-max 16384)
(setq history-length 1000)
(setq use-package-debug nil)
(setq use-package-verbose t)

;;; Packages
;; ==========

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)

;; optimization, no need to activate all the packages so early
(setq package-enable-at-startup nil)
(setq package-user-dir (concat d12/cache-directory "elpa"))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Requires
;; ----------

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

;;; Navigation configurations
;; ===========================

(ido-mode t)
(setq ido-save-directory-list-file (concat d12/cache-directory "ido.last")
      ;; enable fuzzy matching
      ido-enable-flex-matching t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; don't fight with me!
(custom-set-variables '(ring-bell-function 'ignore))
(setq visible-bell nil)

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; When point is on paranthesis, highlight the matching one
(show-paren-mode t)

;;; Edit configurations
;; =====================

;; explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; no welcome buffer
(setq inhibit-startup-screen t)

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              default-tab-width 2)

(setq longlines-show-hard-newlines t)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; UI configurations
;; ===================

;; Show column number in mode line
(setq column-number-mode t)

;; line number
(setq linum-format "%4d")

;; highlight current line
(global-hl-line-mode t)

;; no blink
(blink-cursor-mode 0)

;; When emacs asks for "yes" or "no", let "y" or "n" sufficide
(fset 'yes-or-no-p 'y-or-n-p)

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

; removes the GUI elements
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))

 ;; tooltips in echo-aera
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))
(setq tooltip-use-echo-area t)
(unless (eq window-system 'mac)
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1)))

;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;;; Session configurations
;; ========================

;; scratch buffer empty
(setq initial-scratch-message nil)

;; don't create backup~ or #auto-save# files
(setq backup-by-copying t
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t
              save-place-file (concat d12/cache-directory "places"))

;; minibuffer history
(require 'savehist)
(setq savehist-file (concat d12/cache-directory "savehist")
      enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history)
      savehist-autosave-interval 60)
(savehist-mode +1)

;; auto-save
(let
    ((autosave-dir (expand-file-name (concat d12/cache-directory "autosave"))))
  (unless (file-exists-p autosave-dir)
    (make-directory autosave-dir))
  (setq auto-save-list-file-prefix (concat autosave-dir "/")
        auto-save-file-name-transforms `((".*" ,autosave-dir t))))

;; cache files
;; bookmarks
(setq bookmark-default-file (concat d12/cache-directory "bookmarks")
      ;; save after every change
      bookmark-save-flag 1
      url-configuration-directory (concat d12/cache-directory "url")
      eshell-directory-name (concat d12/cache-directory "eshell" )
      tramp-persistency-file-name (concat d12/cache-directory "tramp"))

;; increase memory threshold for GC
(setq gc-cons-threshold 20000000)

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; Packages
;; ==========

;;; Utilities
;; -----------

(use-package dash
  :ensure t)

(use-package s
  :ensure t
  :defer 1)

;;; Fonts
;; ------

;; Select best available font
(use-package dynamic-fonts
  :ensure t
  :config
  (progn
    (setq dynamic-fonts-preferred-monospace-fonts
          '("Anonymous Pro" ; http://www.marksimonson.com/fonts/view/anonymous-pro
            "Fira Mono"
            "Menlo")
          dynamic-fonts-preferred-monospace-point-size 12

          dynamic-fonts-preferred-proportional-fonts
          '("Fira Sans" ; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
            "Helvetica")
          dynamic-fonts-preferred-proportional-point-size 12)

    (dynamic-fonts-setup)))

;; Map Unicode blocks to fonts
;; currently disabled
;; todo - investigate this package more
(use-package unicode-fonts
  :ensure t
  :disabled t
  :init (unicode-fonts-setup))

;;; Themes
;; --------

(use-package leuven
  :ensure leuven-theme
  :defer t
  :init (load-theme 'leuven t)
  :config (set-face-attribute hl-line-face nil :underline nil))

(use-package solarized
  :disabled
  :ensure solarized-theme
  :defer t
  :init (load-theme 'solarized-light 'no-confirm))

(use-package zenburn
  :disabled t
  :ensure zenburn-theme
  :defer t
  :init (load-theme 'zenburn 'no-confirm))

;;; Various
;; ---------

(use-package fancy-battery              ; Fancy battery info for mode line
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(add-hook 'window-setup-hook 'toggle-frame-maximized)

;;; Mode line
;; -----------

;; todo - implement it

;;; Other configurations
;; ======================

(-each d12/custom-configs
  (lambda (config)
    (d12/load-config config)))
