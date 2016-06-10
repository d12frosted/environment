;;; init.el --- Spacemacs dotfile
;;;
;;; Commentary:
;;
;; This file is loaded by Spacemacs at startup.
;;
;;; Code:

;; setup some constants and variables
(defconst d12-path/dropbox (concat user-home-directory "Dropbox/"))
(defconst d12-path/xdg-config (concat (getenv "XDG_CONFIG_HOME") "/"))
(defconst d12-path/emacs-home (expand-file-name user-emacs-directory))
(defconst d12-path/emacs-layers (concat d12-path/xdg-config "emacs/"))
(defconst d12-path/emacs-private (concat d12-path/dropbox "Apps/Emacs/"))
(defconst d12-path/fish-public (concat d12-path/xdg-config "fish/"))
(defconst d12-path/fish-private (concat d12-path/dropbox "Apps/fish/"))
(defconst d12-path/developer (concat user-home-directory "Developer/"))
(defconst d12-path/org-home (concat d12-path/dropbox "org/"))

;; setup custom-file
(setq custom-file (concat d12-path/emacs-private "custom.el"))
(load custom-file t)

;; load `private.el' file containing all the sensitive data
(load (concat d12-path/emacs-private "private.el"))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path `(,dotspacemacs-directory)
   dotspacemacs-configuration-layers
   '(;; utilities
     (auto-completion :disabled-for org)
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

     ;; private layers
     no-dots
     d12frosted-core
     d12frosted-org
     d12frosted-haskell
     d12frosted-lua
     d12frosted-visual

     ;; languages
     emacs-lisp
     erlang
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

     ;; frameworks
     react)
   dotspacemacs-additional-packages '(color-theme-sanityinc-tomorrow
                                      apropospriate-theme
                                      reveal-in-osx-finder
                                      vimish-fold)
   dotspacemacs-excluded-packages '(emmet-mode
                                    window-numbering)
   dotspacemacs-download-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   spacemacs-buffer-logo-title "[A N I M A C S]"
   dotspacemacs-startup-banner (concat d12-path/emacs-private "animacs-banner.png")
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-startup-recent-list-size 16
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
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-enable-paste-micro-state t
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
   dotspacemacs-smartparens-strict-mode nil
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
  (configuration-layer//declare-layers)
  (mapc #'d12-layers/add-extra-to-load-path
        (configuration-layer/get-layers-list))
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

(defun d12-layers/add-extra-to-load-path (layer)
  "Add 'extra' folder to `load-path' for a given LAYER.

Load-path is modified only when such folder exists."
  (let* ((layer-path (configuration-layer/get-layer-path layer))
         (layer-root (format "%s%s/" layer-path layer))
         (extra-path (concat layer-root "extra/")))
    (when (file-exists-p extra-path)
      (add-to-load-path extra-path))))

(defmacro d12-layers|define-layer-package (layer package &rest args)
  "Define package for specified layer."
  (let* ((fname (intern (format "%s/init-%s"
                               layer
                               package)))
         (feature-init (intern (format "%s-%s-init" layer package)))
         (feature-config (intern (format "%s-%s-config" layer package)))
         (package-config args))
    (add-to-list 'package-config :init t)
    (add-to-list 'package-config `(require ',feature-init nil t) t)
    (add-to-list 'package-config :config t)
    (add-to-list 'package-config `(require ',feature-config nil t) t)
    (message "feature-init = %s" feature-init)
    `(defun ,fname ()
       (use-package ,package ,@package-config))))

;;; init.el ends here
