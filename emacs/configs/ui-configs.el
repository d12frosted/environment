;;; ui-configs.el --- configs file of ui configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 11 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;; Show column number in mode line
(setq column-number-mode t)

;; line number
;; actually I rarelly use linum
;; but when I do
;; I want it to be configured
(setq linum-format "%4d")

;; highlight current line
(global-hl-line-mode t)

;; no blink
;; let's say no to PSE
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

;;; Fonts
;; =======

;; select best available font
(use-package dynamic-fonts
  :ensure t
  :config
  (progn
    (setq dynamic-fonts-preferred-monospace-fonts
          '("M+ 1m"           ; http://mplus-fonts.osdn.jp/mplus-outline-fonts/index-en.html
            "Source Code Pro" ; https://github.com/adobe-fonts/source-code-pro
            "Anonymous Pro"   ; http://www.marksimonson.com/fonts/view/anonymous-pro
            "Menlo")
          dynamic-fonts-preferred-monospace-point-size 12

          dynamic-fonts-preferred-proportional-fonts
          '("Fira Sans" ; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
            "Helvetica")
          dynamic-fonts-preferred-proportional-point-size 12)

    (dynamic-fonts-setup)))

;; Map Unicode blocks to fonts
;; don't forget to download and install:
;; * http://dejavu-fonts.org/wiki/Download
;; * http://www.quivira-font.com/downloads.php
;; * http://www.google.com/get/noto/#/
;; * http://users.teilar.gr/%7Eg1951d/
(use-package unicode-fonts
  :ensure t
  :disabled t
  :defer 1
  :config (unicode-fonts-setup))

;;; Themes
;; ========
;;
;; previously I was using different themes based on my mood
;; and also I was using different themes for terminal emacs and GUI
;; but leuven conquered me
;; it all started with org-mode (if you know what I mean)
;; and then I found that it's the best theme for eveyrthing

(use-package leuven
  :ensure leuven-theme
  :init
  (load-theme 'leuven t)
  (set-face-attribute hl-line-face nil :underline nil)
  (setq d12/shm-current-face-bg "#F6FECD"
        d12/shm-quarantine-face-bg "#FBE3E4"))

;;; Various pretty packages
;; =========================

(use-package nyan-mode
  :ensure t
  :defer t
  :init
  (d12|add-toggle nyan-cat-progress-bar
                  :status nyan-mode
                  :on (nyan-mode)
                  :off (nyan-mode -1)
                  :documentation "Show a nyan cat progress bar in the mode-line."
                  :bind-global "C-c t n")
  (nyan-mode))

;; Fontify number literals
(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Fontify color values in code
(use-package rainbow-mode
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

;;; Mode line
;; ===========

;; because this is awesome
;; just AWESOME
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme nil)
  (sml/setup))

;;; Scratch buffer
;; ================

;; scratch buffer might be empty
;; but we all love cats, so...
(setq initial-scratch-message
      ";; ((ネコ)
;;
;;           |`-.._____..-'|
;;           :  > .  ,  <  :
;;           `./ __`' __ \\,'
;;            | (|_) (|_) |
;;            ; _  .  __  :
;;            `.,' - `-. ,'
;;              `, `_  .'
;;              /       \\
;;             /         :
;;            :          |_
;;           ,|  .    .  | \\
;;          : :   \\   |  |  :
;;          |  \\   :`-;  ;  |
;;          :   :  | /  /   ;
;;           :-.'  ;'  / _,'`------.
;;           `'`''-`'''-'-''--.---  )
;;                        SSt `----'
;;

")
