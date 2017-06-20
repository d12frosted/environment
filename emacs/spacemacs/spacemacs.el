;;; init.el --- Spacemacs dotfile
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; This file is loaded by Spacemacs at startup.
;;
;;; Code:

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; spacemacs--custom-file ""
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path `(,dotspacemacs-directory)
   dotspacemacs-configuration-layers
   '( ;; utilities
     (auto-completion :disabled-for org spacemacs-org)
     better-defaults
     dash
     (git :variables
          git-magit-status-fullscreen t)
     github
     ranger
     spacemacs-layouts
     ivy
     spell-checking
     version-control

     ;; private layers
     d12frosted-core
     d12frosted-csharp
     d12frosted-lisp
     d12frosted-osx
     d12frosted-theming
     d12frosted-visual
     d12frosted-window-purpose

     ;; languages
     csharp
     emacs-lisp
     shell-scripts
     )
   dotspacemacs-additional-packages '(color-theme-sanityinc-tomorrow
                                      apropospriate-theme
                                      reveal-in-osx-finder
                                      smart-tabs-mode
                                      spaceline-all-the-icons
                                      ;; lyrics
                                      elscreen-fr
                                      apples-mode
                                      vimish-fold)

   dotspacemacs-excluded-packages '(emmet-mode
                                    smart-ops
                                    dumb-jump
                                    centered-buffer-mode
                                    emoji-cheat-sheet-plus
                                    )
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; Use my own ELPA archives.
  (setq configuration-layer--elpa-archives
        `(("melpa" . ,(concat d12-path-elpa-mirror-home "melpa/"))
          ("org"   . ,(concat d12-path-elpa-mirror-home "org/"))
          ("gnu"   . ,(concat d12-path-elpa-mirror-home "gnu/"))
          ("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
          ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
          ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

  (setq-default
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   ;; dotspacemacs-elpa-subdirectory emacs-version
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   spacemacs-buffer-logo-title "[A N I M A C S]"
   dotspacemacs-startup-banner (concat d12-path-emacs-private "animacs-banner.png")
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes (if (display-graphic-p)
                           '(spacemacs-light
                             spacemacs-dark
                             apropospriate-dark
                             apropospriate-light
                             leuven)
                         '(sanityinc-tomorrow-eighties
                           apropospriate-dark
                           apropospriate-light
                           spacemacs-light
                           spacemacs-dark
                           sanityinc-tomorrow-eighties))
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               ;; "M+ 1m" ; http://mplus-fonts.osdn.jp/mplus-outline-fonts/index-en.html
                               ;; "Fira Mono"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "H"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose t
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode))
  (setq apples-indent-offset 2)
  ;; # mac 上 emacs 直接编辑二进制applescript
  ;; Allow editing of binary .scpt files (applescript) on mac.
  (add-to-list 'jka-compr-compression-info-list
               `["\\.scpt\\'"
                 "converting text applescript to binary applescprit "
                 ,(executable-find "applescript-helper") nil
                 "converting binary applescript to text applescprit "
                 ,(executable-find "applescript-helper") ("-d")
                 nil t "FasdUAS"])
  ;; It is necessary to perform an update!
  (jka-compr-update)

  (setq-default omnisharp-server-executable-path "/usr/local/bin/omnisharp")
  (setq-default spacemacs-theme-org-highlight t
                spacemacs-theme-org-height t))

(defun dotspacemacs/user-config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; setup M-h on macOS
  (d12/setup-M-h)

  ;; restore sane meaning of M-<number>
  (define-key winum-keymap (kbd "M-0") nil)
  (define-key winum-keymap (kbd "M-1") nil)
  (define-key winum-keymap (kbd "M-2") nil)
  (define-key winum-keymap (kbd "M-3") nil)
  (define-key winum-keymap (kbd "M-4") nil)
  (define-key winum-keymap (kbd "M-5") nil)
  (define-key winum-keymap (kbd "M-6") nil)
  (define-key winum-keymap (kbd "M-7") nil)
  (define-key winum-keymap (kbd "M-8") nil)
  (define-key winum-keymap (kbd "M-9") nil)

  (spacemacs/toggle-camel-case-motion-globally-on))

;; spacemacs.el ends here
