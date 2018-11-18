;;; init.el --- Spacemacs dotfile -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment/emacs
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
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path `(,path-spacemacs-config-home)

   dotspacemacs-configuration-layers '(
                                       ;; private
                                       d12-applescript
                                       d12-core
                                       d12-csharp
                                       d12-emacs-lisp
                                       d12-git
                                       d12-haskell
                                       d12-org
                                       d12-shell
                                       d12-text
                                       d12-visual
                                       d12-web
                                       vulpea

                                       ;; spacemacs
                                       coffeescript
                                       dash
                                       docker
                                       (json :variables
                                             json-reformat:indent-width 2
                                             js-indent-level 2)
                                       latex
                                       ranger
                                       scala
                                       sql
                                       treemacs
                                       typescript
                                       yaml
                                       )

   dotspacemacs-additional-packages '(elscreen-fr
                                      org-cliplink
                                      protobuf-mode
                                      fontawesome
                                      el-mock)

   dotspacemacs-excluded-packages '(emmet-mode
                                    dumb-jump
                                    info+
                                    help-fns+
                                    centered-buffer-mode
                                    emoji-cheat-sheet-plus)

   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; Use my own ELPA archives.
  (if (file-exists-p path-elpa-mirror-home)
      (setq configuration-layer-elpa-archives
            `(("melpa" . ,(concat path-elpa-mirror-home "melpa/"))
              ("org"   . ,(concat path-elpa-mirror-home "org/"))
              ("gnu"   . ,(concat path-elpa-mirror-home "gnu/"))))
    (setq configuration-layer-elpa-archives
        `(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
          ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
          ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/"))))

  (setq-default
   spacemacs-buffer-logo-title "======= D A O M A C S ======"
   spacemacs-cache-directory  path-emacs-cache
   spacemacs-auto-save-directory (concat path-emacs-private "auto-save/")

   ;; dotspacemacs configuration
   dotspacemacs-frame-title-format ""
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   ;; dotspacemacs-elpa-subdirectory emacs-version
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner (concat path-emacs-private "banner.png")
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes (if (display-graphic-p)
                           '(leuven)
                         '(leuven))
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
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
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
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages)
  (setq css-indent-offset 2))

(defun dotspacemacs/user-config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; Restore functionality of M-h on macOS.
  ;; (d12/setup-M-h)
  (spacemacs/toggle-camel-case-motion-globally-on))

(defun dotspacemacs/user-env ())

;; spacemacs.el ends here