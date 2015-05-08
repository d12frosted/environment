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
(setq gc-cons-threshold 20000000)
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

;; I like explicitly declaring what I want
;; (setq use-package-always-ensure t)

;;; Requires
;; ----------

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

;;; Navigation configurations
;; ===========================

;; (ido-mode t)
;; (setq ido-save-directory-list-file (concat d12/cache-directory "ido.last")
;;       ;; enable fuzzy matching
;;       ido-enable-flex-matching t)

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

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; dwim!
(delete-selection-mode 1)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook (lambda nil (diminish 'visual-line-mode)))
(add-hook 'text-mode-hook 'turn-off-auto-fill)

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
;; (setq initial-scratch-message nil)
;; becausse we all love cats
(setq initial-scratch-message
      ";; ((cat)
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

")

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

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package guide-key
  :ensure t
  :defer t
  :diminish guide-key-mode
  :init
  (setq-default d12/guide-prefix "d12/")

  ;; define some prefixes
  (d12|define-prefix "C-c a" applications)
  (d12|define-prefix "C-c b" browse)
  (d12|define-prefix "C-c c" current-mode)
  (d12|define-prefix "C-c o" org-mode)
  (d12|define-prefix "C-c p" projectile)
  (d12|define-prefix "C-c t" toggles)
  (d12|define-prefix "C-c u" utilities)
  (d12|define-prefix "C-c v" version-control)

  ;; highlight 'd12/guide-prefix
  (setq guide-key/highlight-command-regexp d12/guide-prefix)

  ;; guide all keys
  (setq guide-key/guide-key-sequence t)

  (guide-key-mode 1))

(use-package s
  :ensure t
  :defer 1)

(use-package ag
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
          '("Source Code Pro" ; https://github.com/adobe-fonts/source-code-pro
            "Anonymous Pro" ; http://www.marksimonson.com/fonts/view/anonymous-pro
            "Fira Mono"
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
  ;; this is a heavy package, so defer it's loading
  :defer 1
  :config (unicode-fonts-setup))

;;; Themes
;; --------

(use-package leuven
  :ensure leuven-theme
  :defer t)

(use-package solarized
  :disabled t
  :ensure solarized-theme
  :defer t
  :init (load-theme 'solarized-light 'no-confirm))

(use-package zenburn
  :ensure zenburn-theme
  :defer t)

;; leuven looks pretty ugly in my terminal
;; because of my(!) terminal configurations
;; in GUI emacs leuven rocks!
;; don't believe me? TRY IT! IT'S AMAZING!
(if window-system
    (progn (load-theme 'leuven t)
           (set-face-attribute hl-line-face nil :underline nil))
  (progn (load-theme 'zenburn 'no-confirm)))

;;; Helm and friends
;; ------------------

(use-package helm
  :ensure t
  :defer t
  :diminish helm-mode
  :bind (("C-x C-f" . helm-for-files)
         ("C-c b f" . helm-find-files)
         ("C-c b l" . helm-locate)
         ("C-x b" . helm-buffers-list)
         ("C-c b b" . helm-buffers-list)
         ("C-c b r" . helm-recentf)
         ("C-c b s" . helm-mini)
         ("C-c b /" . helm-restore)
         ("M-x" . helm-M-x))
  :init
  (setq helm-quick-update t
        helm-idle-delay 0.01
        helm-input-idle-delay 0.01
        helm-adaptive-history-file (concat d12/cache-directory "helm-adaptive-history"))

  ;; disable popwin-mode in an active Helm session It should be disabled
  ;; otherwise it will conflict with other window opened by Helm persistent
  ;; action, such as *Help* window.
  (add-hook 'helm-after-initialize-hook (lambda () (popwin-mode -1)))

  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))

  :config
  (require 'helm-config)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  (helm-mode 1)
  ;; (helm-autoresize-mode 1)
  (helm-adaptative-mode 1)
  )

(use-package helm-ag
  :ensure t
  :defer t)

;; I am not sure that I really need it
;; todo - remove it
(use-package helm-mode-manager
  :ensure t
  :defer t
  :bind (("C-c t m m" . helm-switch-major-mode)
         ("C-c t m e" . helm-enable-minor-mode)
         ("C-c t m d" . helm-disable-minor-mode)))

;;; Projectile
;; ------------

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (setq-default projectile-enable-caching nil)
  (setq projectile-sort-order 'recentf)
  (setq projectile-cache-file (concat d12/cache-directory
                                      "projectile.cache"))
  (setq projectile-known-projects-file (concat d12/cache-directory
                                               "projectile-bookmarks.eld"))
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :defer t
  :init
  (setq projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on))

;;; Various
;; ---------

(use-package elfeed-web
  :ensure t
  :defer t
  :init
  :config)

(use-package elfeed
  :ensure t
  :defer 1
  :bind ("C-c a e" . elfeed)
  :config
  (setq elfeed-feeds '(("http://www.reddit.com/r/haskelltil/.rss" haskell reddit)
                       ("http://www.reddit.com/r/haskell/.rss" haskell reddit)
                       ("http://www.reddit.com/r/orgmode/.rss" emacs org-mode reddit)
                       ("http://planet.haskell.org/rss20.xml" haskell)
                       ("http://www.reddit.com/r/emacs/.rss" emacs reddit)
                       ("http://nullprogram.com/feed/" emacs))))

;; Ignore uninteresting files everywhere
(use-package ignoramus
  :ensure t
  :init (ignoramus-setup))

;; Save recently visited files
(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-save-file (concat d12/cache-directory "recentf")
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)))

(use-package fancy-battery
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Fontify number literals
(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; Fontify color values in code
(use-package rainbow-mode
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package restclient                ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-restclient)))

(use-package nyan-mode
  ;; i like it
  ;; but it takes too much space in mode line
  :disabled t
  :ensure t
  :defer t
  :init
  (nyan-mode)
  (d12|add-toggle nyan-cat-progress-bar
                  :status nyan-mode
                  :on (nyan-mode)
                  :off (nyan-mode -1)
                  :documentation "Show a nyan cat progress bar in the mode-line."
                  :bind-global "C-c t n"))

(use-package google-translate
  :ensure t
  :defer 2
  :commands (google-translate-query-translate
             google-translate-at-point
             google-translate-query-translate-reverse
             google-translate-at-point-reverse)
  :bind (("C-c u g Q" . google-translate-query-translate-reverse)
         ("C-c u g q" . google-translate-query-translate)
         ("C-c u g T" . google-translate-at-point-reverse)
         ("C-c u g t" . google-translate-at-point))
  :init
  (d12|define-prefix "C-c u g" google-translate)
  :config
  (setq google-translate-default-source-language "uk"
        google-translate-default-target-language "en"
        google-translate-show-phonetic t))

;;; Languages
;; -----------

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook
            (lambda () (run-hooks 'prog-mode-hook))))

;;; Mode line
;; -----------

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-remote
                mode-line-modified
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                mode-line-position
                (projectile-mode projectile-mode-line)
                " "
                mode-line-modes

                ;; Flycheck status
                ;; (flycheck-mode flycheck-mode-line)

                ;; sometimes this thing is too damn huge
                ;; so I put it into the very end
                (vc-mode vc-mode)))

;;; Other configurations
;; ======================

(d12|diminish abbrev-mode "")

(add-hook 'window-setup-hook 'toggle-frame-maximized)

(bind-keys
 ;; I need this as water!
 ("M-g g" . d12/goto-line-and-center)
 ("C-a" . d12/smart-move-beginning-of-line))

(-each d12/custom-configs
  (lambda (config)
    (d12/load-config config)))
