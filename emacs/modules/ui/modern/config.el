;;; ui/modern/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Nov 2018
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

(defvar +modern-theme nil
  "A symbol representing the Emacs theme to load at startup.

This is changed when `load-theme' is used as well.")

(defvar +modern-font (font-spec :family "Source Code Pro" :size (+modern-font-size))
  "The default font to use.

Expects either a `font-spec', font object, an XFT font string or
an XLFD font string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq +modern-font (font-spec :family \"Fira Mono\" :size 12))
  (setq +modern-font \"Terminus (TTF):pixelsize=12:antialias=off\")")

(defvar +modern-big-font (font-spec :family "Source Code Pro" :size (+modern-big-font-size))
  "The font to use when `+modern-big-font-mode' is enabled.
Expects either a `font-spec' or a XFT font string. See
`+modern-font' for examples.")

(defvar +modern-variable-pitch-font nil
  "The font to use for variable-pitch text.

Expects either a `font-spec', font object, a XFT font string or
XLFD string. See `+modern-font' for examples.

It is recommended you don't set specify a font-size, as to
inherit `+modern-font's size.")

(defvar +modern-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.

Expects either a `font-spec', font object, a XFT font string or
XLFD string. See `+modern-font' for examples.

It is recommended you don't set specify a font-size, as to
inherit `+modern-font's size.")

(defvar +modern-unicode-font nil
  "Fallback font for unicode glyphs. Is ignored if :feature unicode is active.

Expects either a `font-spec', font object, a XFT font string or
XLFD string. See `+modern-font' for examples.

It is recommended you don't set specify a font-size, as to
inherit `+modern-font's size.")

(defvar +modern-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(setq-default
 custom-theme-directory (expand-file-name "themes/" nucleus-emacs-dir)

 ansi-color-for-comint-mode t

  ;; kill compilation process before starting another
 compilation-always-kill t

 ;; save all buffers on `compile'
 compilation-ask-about-save nil

 ;; automatically scroll to the first error
 compilation-scroll-output 'first-error

 ;; please ask me before creating non-existing stuff
 confirm-nonexistent-file-or-buffer t

 ;; remove continuation arrow on right fringe
 fringe-indicator-alist
 (delq (assq 'continuation fringe-indicator-alist)
       fringe-indicator-alist)

 ;; do not comact font caches during GC as we are going to use some heavy fonts
 inhibit-compacting-font-caches t

 ;; just a bigger mini-window
 max-mini-window-height 0.3

 ;; disable mode-line mouseovers
 mode-line-default-help-echo nil

 ;; minibuffer resizing
 resize-mini-windows 'grow-only

 ;; favor vertical splits
 split-height-threshold 120
 split-width-threshold 160

 ;; better buffer name uniquification style
 uniquify-buffer-name-style 'forward

 ;; always avoid GUI (even in GUI)
 use-dialog-box nil

 ;; avoid resizing during font changes
 frame-inhibit-implied-resize t

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
 bidi-display-reordering nil

 ;; hide curosrs in other windoes
 cursor-in-non-selected-windows nil

 enable-recursive-minibuffers nil)

;; `all-the-icons'
(def-package! all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :init
  (defun +modern*disable-all-the-icons-in-tty (orig-fn &rest args)
    (if (display-graphic-p)
        (apply orig-fn args)
      ""))
  :config
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon
                all-the-icons-material
                all-the-icons-faicon
                all-the-icons-fileicon
                all-the-icons-wicon
                all-the-icons-alltheicon))
    (advice-add fn :around #'+modern*disable-all-the-icons-in-tty)))

(def-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp
        "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(def-package! highlight-escape-sequences
  :hook ((prog-mode conf-mode) . highlight-escape-sequences-mode))

(def-package! rainbow-delimiters
  :init
  (setq rainbow-delimiters-max-face-count 3))

(defvar +modern-solaire-themes '()
  "An alist of themes that support `solaire-mode'.

If CDR is t, then use `solaire-mode-swap-bg'.")

(defun +modern-mark-solaire-theme (theme)
  "Mark THEME as one that supports `solaire-mode'."
  (add-to-list '+modern-solaire-themes (cons theme t)))

(def-package! solaire-mode
  :defer t
  :init
  (defun +modern|solaire-mode-swap-bg-maybe ()
    (when-let* ((rule (assq +modern-theme +modern-solaire-themes)))
      (require 'solaire-mode)
      (if (cdr rule) (solaire-mode-swap-bg))))
  (add-hook '+modern-load-theme-hook #'+modern|solaire-mode-swap-bg-maybe t)
  :config
  (add-hook 'change-major-mode-after-body-hook #'turn-on-solaire-mode)
  ;; fringe can become unstyled when deleting or focusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  ;; Prevent color glitches when reloading either Emacs or loading a new theme
  (add-hook! :append '(+modern-load-theme-hook nucleus-reload-hook)
    #'solaire-mode-reset)
  ;; org-capture takes an org buffer and narrows it. The result is erroneously
  ;; considered an unreal buffer, so solaire-mode must be restored.
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)

  ;; Because fringes can't be given a buffer-local face, they can look odd, so
  ;; we remove them in the minibuffer and which-key popups (they serve no
  ;; purpose there anyway).
  (defun +modern|disable-fringes-in-minibuffer (&rest _)
    (set-window-fringes (minibuffer-window) 0 0 nil))
  (add-hook 'solaire-mode-hook #'+modern|disable-fringes-in-minibuffer)

  (defun +modern*no-fringes-in-which-key-buffer (&rest _)
    (+modern|disable-fringes-in-minibuffer)
    (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil))
  (advice-add 'which-key--show-buffer-side-window :after #'+modern*no-fringes-in-which-key-buffer)

  (add-hook! '(minibuffer-setup-hook window-configuration-change-hook)
    #'+modern|disable-fringes-in-minibuffer))

;; `whitespace-mode'
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline
        newline-mark trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

;;
;; Theme & font

(defvar +modern-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or
reloaded with `+modern/reload-theme'.")

(defun +modern*load-theme-hooks (theme &rest _)
  "Set up `+modern-load-theme-hook' to run after `load-theme' is
called."
  (setq +modern-theme theme)
  (run-hooks '+modern-load-theme-hook))
(advice-add #'load-theme :after #'+modern*load-theme-hooks)

(defvar +modern-last-window-system
  (if (daemonp) 'daemon initial-window-system)
  "The `window-system' of the last frame. If this doesn't match the current
frame's window-system, the theme will be reloaded.")

(defun +modern|init-fonts ()
  "Initialize fonts."
  (condition-case e
      (progn
        (cond (+modern-font
               ;; We avoid `set-frame-font' for performance reasons.
               ;; Manipulating `default-frame-alist' is effective enough.
               (add-to-list
                'default-frame-alist
                (cons 'font
                      (cond ((stringp +modern-font) +modern-font)
                            ((fontp +modern-font) (font-xlfd-name +modern-font))
                            ((signal 'wrong-type-argument (list '(fontp stringp) +modern-font)))))))
              ((display-graphic-p)
               (setq +modern-font (face-attribute 'default :font))))
        (when +modern-serif-font
          (set-face-attribute 'fixed-pitch-serif nil :font +modern-serif-font))
        (when +modern-variable-pitch-font
          (set-face-attribute 'variable-pitch nil :font +modern-variable-pitch-font))
        ;; Fallback to `+modern-unicode-font' for Unicode characters
        (when (fontp +modern-unicode-font)
          (set-fontset-font t nil +modern-unicode-font nil 'append)))
    ((debug error)
     (if (string-prefix-p "Font not available: " (error-message-string e))
         (lwarn '+modern-ui :warning
                "Could not find the '%s' font on your system, falling back to system font"
                (font-get (caddr e) :family))
       (signal '+modern-error e)))))

(defun +modern|init-theme ()
  "Set the theme and load the font, in that order."
  (when (and +modern-theme (not (memq +modern-theme custom-enabled-themes)))
    (load-theme +modern-theme t)))

;; Getting themes to remain consistent across GUI Emacs, terminal Emacs and
;; daemon Emacs is hairy. `+modern|init-theme' sorts out the initial GUI frame.
;; Attaching `+modern|init-theme-in-frame' to `after-make-frame-functions' sorts
;; out daemon and emacsclient frames.
;;
;; There will still be issues with simultaneous gui and terminal (emacsclient)
;; frames, however. There's always `+modern/reload-theme' if you need it!
(defun +modern|reload-theme-in-frame-maybe (frame)
  "Reloads the theme in new daemon or tty frames."
  (when (and +modern-theme
             (framep frame)
             (not (eq +modern-last-window-system (framep-on-display frame))))
    (with-selected-frame frame
      (load-theme +modern-theme t))
    (setq +modern-last-window-system (framep-on-display frame))))

(defun +modern|reload-theme-maybe (_frame)
  "Reloads the theme after closing the last frame of a type."
  (unless (cl-find +modern-last-window-system (frame-list) :key #'framep-on-display)
    (setq +modern-last-window-system nil)
    (+modern|reload-theme-in-frame (selected-frame))))

;; fonts
(add-hook '+modern-init-ui-hook #'+modern|init-fonts)

;; themes
(unless (daemonp)
  (add-hook '+modern-init-ui-hook #'+modern|init-theme))
(add-hook 'after-make-frame-functions #'+modern|reload-theme-in-frame-maybe)
(add-hook 'after-delete-frame-functions #'+modern|reload-theme-maybe)

;;
;; Bootstrap

;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; Truly silence startup message
(fset #'display-startup-echo-area-message #'ignore)

;; disable cursort blinking
(blink-cursor-mode -1)

;; Less clutter
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; relegate tooltips to echo area only
(if (boundp 'tooltip-mode) (tooltip-mode -1))

;; Handle ansi codes in compilation buffer
(add-hook 'compilation-filter-hook
          #'+modern|apply-ansi-color-to-compilation-buffer)

;; show typed keystrokes in minibuffer
(defun +modern|enable-ui-keystrokes ()  (setq echo-keystrokes 0.02))
(defun +modern|disable-ui-keystrokes () (setq echo-keystrokes 0))
(+modern|enable-ui-keystrokes)
;; ...but hide them while isearch is active
(add-hook 'isearch-mode-hook     #'+modern|disable-ui-keystrokes)
(add-hook 'isearch-mode-end-hook #'+modern|enable-ui-keystrokes)

(defun +modern|highlight-non-default-indentation ()
  "Highlight whitespace that doesn't match your
`indent-tabs-mode' setting."
  (unless (or (bound-and-true-p global-whitespace-mode)
              (bound-and-true-p whitespace-mode)
              (eq indent-tabs-mode (default-value 'indent-tabs-mode))
              (eq major-mode 'fundamental-mode)
              (derived-mode-p 'special-mode))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (if (or (bound-and-true-p whitespace-mode)
                 (bound-and-true-p whitespace-newline-mode))
             (cl-union (if indent-tabs-mode '(tabs tab-mark) '(spaces space-mark))
                       whitespace-style)
           `(face ,@(if indent-tabs-mode '(tabs tab-mark) '(spaces space-mark))
             trailing-lines tail)))
    (whitespace-mode +1)))

(defun +modern|init-ui ()
  "Initialize UI by applying all its advice and hooks."
  (add-hook 'after-change-major-mode-hook #'+modern|highlight-non-default-indentation)
  (run-hook-wrapped '+modern-init-ui-hook #'nucleus-try-run-hook))

(add-hook 'emacs-startup-hook #'+modern|init-ui)

;;
;; Hacks

;; doesn't exist in terminal Emacs; we define it to prevent errors
(unless (fboundp 'define-fringe-bitmap)
  (defun define-fringe-bitmap (&rest _)))

(defun +modern*disable-old-themes-first (orig-fn &rest args)
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args)
  (when (fboundp 'powerline-reset)
    (powerline-reset)))
(advice-add #'load-theme :around #'+modern*disable-old-themes-first)

(defun +modern*prefer-compiled-theme (orig-fn &rest args)
  "Make `load-theme' prioritize the byte-compiled theme (if it
exists) for a moderate boost in startup (or theme switch) time."
  (cl-letf* ((old-locate-file (symbol-function 'locate-file))
             ((symbol-function 'locate-file)
              (lambda (filename path &optional _suffixes predicate)
                (funcall old-locate-file filename path '("c" "") predicate))))
    (apply orig-fn args)))
(advice-add #'load-theme :around #'+modern*prefer-compiled-theme)

(defun +modern|disable-whitespace-mode-in-childframes (frame)
  "`whitespace-mode' inundates child frames with whitspace
markers, so disable it to fix all that visual noise."
  (when (frame-parameter frame 'parent-frame)
    (with-selected-frame frame
      (setq-local whitespace-style nil)
      frame)))
(add-hook 'after-make-frame-functions #'+modern|disable-whitespace-mode-in-childframes)

(defun +modern*silence-motion-errors (orig-fn &rest args)
  "Prevent disruptive motion errors taking over the minibuffer
while we're in it."
  (if (not (minibufferp))
      (apply orig-fn args)
    (ignore-errors (apply orig-fn args))
    (when (<= (point) (minibuffer-prompt-end))
      (goto-char (minibuffer-prompt-end)))))
(advice-add #'left-char :around #'+modern*silence-motion-errors)
(advice-add #'right-char :around #'+modern*silence-motion-errors)
(advice-add #'delete-backward-char :around #'+modern*silence-motion-errors)
(advice-add #'backward-kill-sentence :around #'+modern*silence-motion-errors)

;; Switch to `+buffer-fallback' if on last real buffer
(advice-add #'kill-this-buffer :around #'+buffer*switch-to-fallback-maybe)

;; fix background in terminal
(defun +modern|patch-background-tty ()
  "Patch background in terminal."
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook #'+modern|patch-background-tty)
