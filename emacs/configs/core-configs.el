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

;;; Variables and constants
;; =========================

;; A bit about myself
(setq user-full-name "Boris Buliga")
(setq user-mail-address "d12frosted@icloud.com")
(setq user-github-url "https://github.com/d12frosted")
(setq user-home-url "http://d12frosted.github.io")

(defconst d12/guide-prefix "d12/"
  "Prefix for guide-key prefixes! Because prefix.")

(defconst d12/cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Storage area for persistent files.")

(defconst d12/packages-directory
  (expand-file-name (concat user-emacs-directory "packages/"))
  "Manually installed packages directory.")

(defconst pcache-directory
  (concat d12/cache-directory "pcache"))

;; create d12/cache-directory if it doesn't exist yet
(unless (file-exists-p d12/cache-directory)
  (make-directory d12/cache-directory))

;;; Custom configs
;; ---------------

(defvar d12/custom-configs '()
  "List of custom config names to load.")

(setq d12/custom-configs
      '("ui"
        "org"
        "magit"
        "elfeed"
        "syntax-checking"
        "mu4e"
        "auto-completion"
        "omnisharp"
        "lisp"))

;;; Packages
;; ==========
;;
;; Configure package.el
;; and bootstrap use-package

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)

;; optimization, no need to activate all the packages so early
;; ~ the question - should I even use it?
(setq package-enable-at-startup nil)

;; download and install packages to cache directory
(setq package-user-dir (concat d12/cache-directory "elpa"))

;; add MELPA to package archives
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; add org archive to package archives
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
;;
;; some important requires

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

;; when there is selection (region) active
;; replace it when typing something
;; instead of placing it before the selection
(delete-selection-mode 1)

;; use visual-line mode for text-mode
(add-hook 'text-mode-hook 'visual-line-mode)

;; turn off auto fill for text0mode
(add-hook 'text-mode-hook 'turn-off-auto-fill)

;; diminish visual-line-mode
(use-package simple
  :diminish visual-line-mode)

;; do not warn me about defadvice
(setq ad-redefinition-action 'accept)

;;; Session configurations
;; ========================

;; scratch buffer is empty
;; for now :D
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
;;
;; confifgurations for different packages
;; just a note - try to move things to separate files
;; when it's possible
;; also, try to keep things unrelated as much as possible
;; so the order of loading doesn't affect the result

;;; Utilities
;; -----------
;;
;; random important stuff that is required
;; by other configurations

;; must-have for list manipulations
(use-package dash
  :ensure t)

;; get's all required environment variables
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; guide-key
;; at this point I find it useful for discovering keys
;; but also checkout helm-descbinds
;; todo - move it out from here
(use-package guide-key
  :ensure t
  :defer t
  :diminish guide-key-mode
  :init
  ;; define some prefixes
  (d12|define-prefix-global "C-c a" applications)
  (d12|define-prefix-global "C-c b" browse)
  (d12|define-prefix-global "C-c c" current-mode)
  (d12|define-prefix-global "C-c o" org-mode)
  (d12|define-prefix-global "C-c p" projectile)
  (d12|define-prefix-global "C-c t" toggles)
  (d12|define-prefix-global "C-c u" utilities)
  (d12|define-prefix-global "C-c v" version-control)

  ;; highlight 'd12/guide-prefix
  (setq guide-key/highlight-command-regexp d12/guide-prefix)

  ;; guide all keys
  (setq guide-key/guide-key-sequence t)

  (guide-key-mode 1))

;; some string manupilations
;; but this is heavy thing - so defer it's loading
(use-package s
  :ensure t
  :defer 1)

;; search tool
(use-package ag
  :ensure t
  :defer 1)

;; ignore uninteresting files everywhere
;; todo - check if it's really needed
(use-package ignoramus
  :ensure t
  :init (ignoramus-setup))

;; save recently visited files
(use-package recentf
  :ensure t
  :defer t
  :init
  ;; lazy load recentf
  (add-hook 'find-file-hook
            (lambda () (unless recentf-mode
                         (recentf-mode)
                         (recentf-track-opened-file))))
  :config
  (setq recentf-exclude '(d12/cache-directory))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude #'ignoramus-boring-p)
  (setq recentf-save-file (concat d12/cache-directory "recentf")
        recentf-max-saved-items 100
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)))

;;; Helm and friends
;; ------------------

(use-package helm
  :ensure t
  :defer t
  :diminish helm-mode
  :bind (("C-x C-f" . helm-for-files)
         ("C-x b"   . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("C-c h C-f" . helm-find-files)
         ("C-c h C-m" . helm-mini))
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
  (helm-adaptative-mode 1))

(use-package helm-ag
  :ensure t
  :defer t)

;; I am not sure that I really need it
;; todo - remove it
(use-package helm-mode-manager
  :ensure t
  :defer t
  :bind
  (("C-c m m" . helm-switch-major-mode)
   ("C-c m e" . helm-enable-minor-mode)
   ("C-c m d" . helm-disable-minor-mode))
  :init
  (d12|define-prefix-global "C-c b m" mode-manager))

(use-package helm-descbinds
  :ensure t
  :defer t
  :init
  ;; lazy load descbinds
  (add-hook 'find-file-hook
            (lambda () (unless helm-descbinds-mode
                         (helm-descbinds-mode)))))

(use-package helm-company
  :ensure t
  :defer t
  :init
  (eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company))))

;;; Projectile
;; ------------

(use-package projectile
  :ensure t
  :defer 1
  :diminish projectile-mode
  :init
  (setq-default projectile-enable-caching nil)

  (setq projectile-sort-order 'recentf

        ;; it's not needed in my setup
        ;; but who cares
        projectile-cache-file (concat d12/cache-directory
                                      "projectile.cache")

        projectile-known-projects-file (concat d12/cache-directory
                                               "projectile-bookmarks.eld"))

  :config
  (projectile-global-mode)
  (setq projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on))

;;; Various
;; ---------

(use-package elec-pair
  :init (electric-pair-mode))

(use-package restclient                ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-restclient)))

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
         ("C-c u g t" . google-translate-at-point)
         ("C-c u g s" . google-translate-smooth-translate))
  :init
  (d12|define-prefix-global "C-c u g" google-translate)
  :config
  (setq google-translate-default-source-language "uk"
        google-translate-default-target-language "en"
        google-translate-show-phonetic t))

(use-package eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :init
  (defvar d12/eldoc-msg-format "")
  (defvar d12/eldoc-msg-args "")

  (setq d12/eldoc-msg-format ""
        d12/eldoc-msg-args '())

  (defun d12/eldoc-message-function (format-string &rest args)
    (setq d12/eldoc-msg-format format-string
          d12/eldoc-msg-args args)
    (d12/update-header-line))

  (setq eldoc-message-function #'d12/eldoc-message-function)
  :config
  ;; enable eldoc in `eval-expression'
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  ;; enable eldoc in IELM
  (add-hook 'ielm-mode-hook #'eldoc-mode))

;;; Languages
;; -----------

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook
            (lambda () (run-hooks 'prog-mode-hook))))

(use-package fish-mode
  :load-path "packages/fish-mode/"
  :mode ("\\.fish$" . fish-mode))

;;; Other configurations
;; ======================

(use-package abbrev
  :diminish abbrev-mode)

(add-hook 'window-setup-hook 'toggle-frame-maximized)

(bind-keys
 ;; whenever I go to line
 ;; I want that line be at the center of window
 ("M-g g" . d12/goto-line-and-center)

 ;; I rarely need to go to the real beginning of line
 ;; usually I need go back to first non-whitespace symbol
 ("C-a" . d12/smart-move-beginning-of-line)

 ;; do what I mean
 ;; usually I remove lines and regions by <M-w>
 ;; but when I don't want to put it to kill-ring
 ;; I use <C-S-backspace>
 ("C-w" . d12/cut-line-or-region)
 ("M-w" . d12/copy-line-or-region)
 ("C-S-<backspace>" . d12/delete-line-or-region)
 ("C-c u d" . d12/duplicate-line-or-region))

;; and load my custom configurations
;; that's the final step for this file
(-each d12/custom-configs
  (lambda (config)
    (d12/load-config config)))

;;; end
