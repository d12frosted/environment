;;; init.el --- Spacemacs dotfile
;;;
;;; Commentary:
;;
;; This file is loaded by Spacemacs at startup.
;;
;;; Code:

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path `(,dotspacemacs-directory)
   dotspacemacs-configuration-layers
   '( ;; utilities
     (auto-completion :disabled-for org
                      :variables
                      auto-completion-enable-help-tooltip t)
     better-defaults
     colors
     dash
     erc
     (git :variables
          git-magit-status-fullscreen t)
     github
     (org :variables
          org-enable-github-support t)
     pandoc
     ranger
     restclient
     syntax-checking
     spacemacs-layouts
     ivy
     spell-checking
     shell
     version-control
     xkcd

     ;; private layers
     no-dots
     d12frosted-core
     d12frosted-csharp
     d12frosted-haskell
     d12frosted-lisp
     d12frosted-lua
     d12frosted-org
     d12frosted-osx
     d12frosted-spellchecking
     d12frosted-theming
     d12frosted-visual
     d12frosted-window-purpose

     ;; languages
     csharp
     emacs-lisp
     (haskell :variables
              haskell-completion-backend 'ghci
              haskell-enable-hindent-style "chris-done")
     (html :variables
           web-mode-markup-indent-offset 2
           web-mode-attr-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-sql-indent-offset 2
           css-indent-offset 2)
     (javascript :variables
                 js-indent-level 2
                 js2-basic-offset 2)
     lua
     markdown
     shell-scripts
     yaml
     shaders

     ;; frameworks
     react)
   dotspacemacs-additional-packages '(color-theme-sanityinc-tomorrow
                                      apropospriate-theme
                                      reveal-in-osx-finder
                                      smart-tabs-mode
                                      lyrics
                                      elscreen-fr
                                      vimish-fold)
   dotspacemacs-excluded-packages '(emmet-mode
                                    dumb-jump
                                    centered-buffer-mode
                                    window-numbering)
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; no home buffer lists centring
  (defun spacemacs-buffer//center-startupify-lists ()
    (message "We don't need no centring."))

  ;; setup elpa archives to my own mirrors
  (setq configuration-layer--elpa-archives
        '(;; ("melpa" . "/Users/d12frosted/Developer/d12frosted/elpa-mirror/melpa/")
          ;; ("org"   . "/Users/d12frosted/Developer/d12frosted/elpa-mirror/org/")
          ;; ("gnu"   . "/Users/d12frosted/Developer/d12frosted/elpa-mirror/gnu/")
          ("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
          ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
          ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

  (setq-default
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   spacemacs-buffer-logo-title "[A N I M A C S]"
   dotspacemacs-startup-banner (concat d12-path/emacs-private "animacs-banner.png")
   dotspacemacs-startup-lists '(agenda (recents . 8) bookmarks)
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
                               :size 10
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Org"
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
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (setq-default
   omnisharp-server-executable-path
   (concat d12-path/developer
           "Other/omnisharp-roslyn/artifacts/publish/OmniSharp/default/net451/OmniSharp.exe"))
  (setq-default spacemacs-theme-org-highlight t
                spacemacs-theme-org-height t))

(defun dotspacemacs/user-config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; TODO: move me out
  (d12/setup-M-h)
  (spacemacs/toggle-camel-case-motion-globally-on)
  (spacemacs/toggle-automatic-symbol-highlight-on))

;;; init.el ends here
