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
   dotspacemacs-configuration-layer-path `(,dotspacemacs-directory)
   dotspacemacs-configuration-layers
   '(;; utilities
     (auto-completion :disabled-for org)
     better-defaults
     colors
     emoji
     eyebrowse
     (git :variables
          git-magit-status-fullscreen t)
     github
     (org :variables
          org-enable-github-support t
          org-bullets-bullet-list '("◉" "○" "✿" "❀" "✸"))
     pandoc
     ;; spacemacs-layouts
     ranger
     restclient
     ;; semantic
     syntax-checking
     spell-checking
     (shell :variables
            shell-default-term-shell "/usr/local/bin/fish")
     version-control

     ;; private layers
     no-dots
     (d12frosted :variables
                 d12/org-home-path (concat d12/dropbox-path "org/")
                 d12/org-author-name `(,d12/full-name)
                 d12/org-author-email `(,d12/email)
                 d12/org-agenda-ignore-dirs '(".git/"
                                              "journal/"))
     (elfeed :variables
             elfeed-feeds d12/elfeed-feeds)

     ;; languages
     (csharp :variables
             omnisharp-server-executable-path
             "~/Developer/Other/omnisharp-roslyn/artifacts/build/omnisharp/omnisharp")
     emacs-lisp
     erlang
     (haskell :variables
              haskell-enable-hindent-style "chris-done"
              haskell-process-type 'stack-ghci
              haskell-tags-on-save nil
              haskell-process-log t
              haskell-process-reload-with-fbytecode nil
              haskell-process-use-presentation-mode t
              haskell-interactive-mode-include-file-name nil
              haskell-interactive-mode-eval-pretty nil
              haskell-process-suggest-haskell-docs-imports nil)
     (html :variables
           web-mode-markup-indent-offset 2
           web-mode-attr-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-sql-indent-offset 2)
     (javascript :variables
                 js-indent-level 2
                 js2-basic-offset 2)
     lua
     markdown
     python
     ruby
     shell-scripts
     yaml)
   dotspacemacs-additional-packages '(comment-dwim-2
                                      color-theme-sanityinc-tomorrow
                                      apropospriate-theme
                                      reveal-in-osx-finder
                                      beacon
                                      vimish-fold
                                      (haskell-mode :location "~/Developer/haskell-mode/"))
   dotspacemacs-excluded-packages '(emmet-mode)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."

  (require 'package)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  ;; make sure that `exec-path-from-shell' is loaded
  (spacemacs/load-or-install-package 'exec-path-from-shell)

  ;; extract values of environment variables
  (exec-path-from-shell-copy-env "XDG_CONFIG_HOME")

  ;; setup path variables
  (setq-default d12/dropbox-path (concat user-home-directory "Dropbox/")
                d12/emacs-private-path (concat d12/dropbox-path "Apps/Emacs/"))

  ;; and load `private.el' file containing all sensitive data
  (load (concat d12/emacs-private-path "private.el"))

  ;; setup custom-file
  (setq custom-file (concat d12/emacs-private-path "custom.el"))
  (load custom-file t)

  (setq-default
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner (concat dotspacemacs-directory "animacs-banner.png")
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               ;; "M+ 1m" ; http://mplus-fonts.osdn.jp/mplus-outline-fonts/index-en.html
                               ;; "Fira Mono"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil

   ;; other
   spacemacs-mode-line-org-clock-current-taskp t))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."

  (setq-default
   ;; Miscellaneous
   vc-follow-symlinks t
   require-final-newline t

   ;; Whitespace mode
   whitespace-style '(face tabs tab-mark)
   whitespace-display-mappings
   '((newline-mark 10 [172 10])
     (tab-mark 9 [9655 9]))

   ;; Ranger
   ranger-override-dired t)

  ;; rename title in init screen
  (defadvice spacemacs-buffer//insert-image-banner (after d12//spacemacs-title-advice activate)
    "Change the default title in *spacemacs* banner."
    (save-excursion
      (goto-char (point-min))
      (search-forward "[S P A C E M A C S]")
      (replace-match "[A N I M A C S]")))

  ;; hooks
  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'after-save-hook 'delete-trailing-whitespace)

  (spacemacs|use-package-add-hook helm
    :post-config
    ;; Disable fuzzy matching to make mdfind work with helm-locate
    (setq helm-locate-fuzzy-match nil)
    (setq helm-locate-command "mdfind -name %s %s"))

  ;; init GUI or terminal
  (if (display-graphic-p)
      (d12//init-gui)
    (d12//init-terminal))

  (setq auto-mode-alist
        (cons '("SConstruct" . python-mode) auto-mode-alist))
  (setq auto-mode-alist
        (cons '("SConscript" . python-mode) auto-mode-alist)))

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  (defmacro remove-from-list (list-var element)
    `(setq ,list-var (remove ,element ,list-var)))

  ;; hooks
  (add-hook 'find-file-hook 'd12/load-dir-settings)
  (add-hook 'company-mode-hook 'company-quickhelp-mode)
  (add-hook 'prog-mode-hook 'vimish-fold-mode)
  (add-hook 'company-quickhelp-mode-hook 'd12//init-company-quickhelp-mode)
  (add-hook 'org-mode-hook 'd12//init-org-mode)
  (add-hook 'haskell-interactive-mode-hook 'd12//init-haskell-interactive-mode)

  ;; haskell indentation
  ;; TODO: remove when #3586 is merged
  ;; https://github.com/syl20bnr/spacemacs/pull/3586
  (spacemacs|define-micro-state haskell-indentation
    :doc "Press = to indent further or - to indent backwards"
    :use-minibuffer t
    :bindings
    ("=" indent-for-tab-command)
    ("-" haskell-indentation-indent-backwards))
  (evil-define-key 'visual haskell-mode-map (kbd "=") 'spacemacs/haskell-indentation-micro-state)

  ;; configs
  (helm-projectile-on)
  (delete-selection-mode 1)
  (beacon-mode 1)

  ;; vars
  (setq projectile-enable-caching nil
        magit-repo-dirs '("~/Developer/")
        google-translate-default-source-language "Ukrainian"
        google-translate-default-target-language "English"
        git-messenger:show-detail t
        haskell-process-suggest-remove-import-lines nil
        haskell-process-suggest-hoogle-imports nil
        powerline-default-separator 'utf-8)

  ;; setup scrolling
  (setq scroll-margin 0                   ; Drag the point along while scrolling
        scroll-conservatively 1000        ; Never recenter the screen while scrolling
        scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
        ;; These settings make trackpad scrolling on OS X much more predictable
        ;; and smooth
        mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(1))

  ;; OS X
  (if (spacemacs/system-is-mac)
      (setq mac-command-modifier 'meta
            mac-option-modifier  'none))
  (bind-key "M-h" 'ns-do-hide-emacs)

  ;; python
  (defun pyenv-mode-versions ()
    "List installed python versions."
    (let ((versions (shell-command-to-string "vf ls")))
      (delete-dups (cons "system" (split-string versions)))))

  ;; key bindings
  (evil-leader/set-key
    "it" 'd12/insert-time
    "id" 'd12/insert-date
    "iD" 'd12/insert-full-date))

(defun configuration-layer/get-owner (pkg &optional print)
  (interactive "SEnter package name: \np")
  (let ((owner (cdr (assoc pkg
                           (mapcar (lambda (pkg)
                                     (cons (oref pkg :name)
                                           (oref pkg :owner)))
                                   configuration-layer--packages)))))
    (when print
      (message "Owner of %S is %S" pkg owner))
    owner))

(defun d12//init-terminal ()
  "Initialization function when `display-graphic-p' returns nil."
  (setq-default dotspacemacs-themes '(
                                      apropospriate-dark
                                      spacemacs-dark
                                      spacemacs-light
                                      sanityinc-tomorrow-eighties
                                      )))

(defun d12//init-gui ()
  "Initialization function when `display-graphic-p' returns t."
  (setq-default dotspacemacs-themes '(
                                      spacemacs-light
                                      spacemacs-dark
                                      apropospriate-dark
                                      apropospriate-light
                                      leuven
                                      )))

(defun d12//init-haskell-interactive-mode ()
  (setq-local evil-move-cursor-back nil))

(defun d12//init-company-quickhelp-mode ()
  (define-key company-quickhelp-mode-map (kbd "M-h") nil))

(defun d12//init-org-mode ()
  (define-key org-mode-map (kbd "M-h") nil))

;;; spacemacs ends here
