;;; packages-config.el --- d12frosted-core layer packages-config file for
;;; Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun d12frosted-core/init-beacon ()
  (use-package beacon
    :init
    (beacon-mode 1)))

(defun d12frosted-core/pre-init-ranger ()
  (use-package ranger
    :init
    (setq-default ranger-override-dired t
                  ranger-show-literal nil)))

(defun d12frosted-core/post-init-google-translate ()
  (use-package google-translate
    :config
    (setq google-translate-default-source-language "uk"
          google-translate-default-target-language "en")))

(defun d12frosted-core/pre-init-projectile ()
  (use-package projectile
    :init
    (setq projectile-enable-caching nil)
    (when (configuration-layer/layer-usedp 'spacemacs-helm)
      (helm-projectile-on))))

(defun d12frosted-core/post-init-projectile ()
  (use-package projectile
    :config
    (setq projectile-switch-project-action #'projectile-commander)
    (def-projectile-commander-method ?s
      "Open a *shell* buffer for the project."
      (shell (get-buffer-create
              (format "*shell %s*"
                      (projectile-project-name)))))
    (def-projectile-commander-method ?c
      "Run `compile' in the project."
      (call-interactively #'compile))
    (def-projectile-commander-method ?\C-?
      "Go back to project selection."
      (projectile-switch-project))
    (def-projectile-commander-method ?d
      "Open project root in dired."
      (projectile-dired))))

(defun d12frosted-core/post-init-spaceline ()
  (use-package spaceline-config
    :init
    (setq powerline-default-separator 'utf-8)
    :config
    (require 'cl)
    (defvar d12-state-cursors '((emacs "SkyBlue2" box)
                                (emacs-input "chartreuse3" box)
                                (god "DarkGoldenrod2" box)
                                (god-input "plum3" box))
          "Colors assigned to several states with cursor definitions.")

    (cl-loop for (state color cursor) in d12-state-cursors
             do
             (eval `(defface ,(intern (format "d12-spaceline-%S-face" state))
                      `((t (:background ,color
                                        :foreground ,(face-background 'mode-line)
                                        :box ,(face-attribute 'mode-line :box)
                                        :inherit 'mode-line)))
                      (format "%s state face." state)
                      :group 'd12frosted))
             (set (intern (format "d12-%s-state-cursor" state))
                  (list (when dotspacemacs-colorize-cursor-according-to-state color)
                        cursor)))

    (defun d12//get-state ()
      (cond
       ((and (bound-and-true-p current-input-method) (bound-and-true-p god-local-mode)) 'god-input)
       ((bound-and-true-p current-input-method) 'emacs-input)
       ((bound-and-true-p god-local-mode) 'god)
       (t 'emacs)))
    (defun d12//get-state-face ()
      (let ((state (d12//get-state)))
        (intern (format "d12-spaceline-%S-face" state))))
    (setq spaceline-highlight-face-func 'd12//get-state-face)))

(defun d12frosted-core/post-init-magit ()
  (use-package magit
    :init
    (setq magit-repo-dirs `(,d12-path/developer))))

(defun d12frosted-core/post-init-git-messenger ()
  (use-package git-messenger
    :config
    (setq git-messenger:show-detail t)))

(when (configuration-layer/layer-usedp 'spacemacs-helm)
  (defun d12frosted-core/post-init-helm ()
    (use-package helm
      :config
      ;; Disable fuzzy matching to make mdfind work with helm-locate
      (setq helm-locate-fuzzy-match nil)
      (setq helm-locate-command "mdfind -name %s %s")
      (bind-key "C-s" 'helm-swoop)
      (bind-key "C-S-s" 'spacemacs/helm-swoop-region-or-symbol))))

(when (configuration-layer/layer-usedp 'spacemacs-ivy)
  (defun d12frosted-core/post-init-ivy ()
    (use-package ivy
      :config
      (d12-ivy//add-interesting-files d12-ivy--config-files)
      (ivy-set-actions
       'counsel-find-file
       '(("d" d12-ivy//delete-file "remove")))
      (bind-key "C-S-s" 'spacemacs/swiper-region-or-symbol))))

(defun d12frosted-core/init-glsl-mode ()
  (use-package glsl-mode
    :mode ("\\.fsh$"  . glsl-mode)
    :mode ("\\.vsh$"  . glsl-mode)
    :mode ("\\.glsl$" . glsl-mode)
    :mode ("\\.frag$" . glsl-mode)
    :mode ("\\.geom$" . glsl-mode)))

;; TODO: move me to separate layer
(defun d12frosted-core/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (defun moai-run-main ()
      "Run main.lua using moai."
      (interactive)
      (setq-local async-shell-command-buffer 'confirm-kill-process)
      (if (projectile-project-p)
          (let ((root (projectile-project-root)))
            (projectile-save-project-buffers)
            (async-shell-command (concat "cd '" root "/lua'; and moai main.lua")))
        (async-shell-command (concat "moai " (buffer-file-name)))))
    (defun moai-run-main-multiplayer ()
      "Run main.lua using moai in multiplayer mode."
      (interactive)
      (setq-local async-shell-command-buffer 'confirm-kill-process)
      (if (projectile-project-p)
          (let ((root (projectile-project-root)))
            (projectile-save-project-buffers)
            (async-shell-command (concat "cd '" root "/lua'; and moai main.lua")))
        (async-shell-command (concat "moai " (buffer-file-name) " -r 5152") "moai-server")
        (async-shell-command (concat "moai " (buffer-file-name) " -c 127.0.0.1 5152") "moai-client")))
    (defun moai-upload ()
      "Upload moai game to device."
      (interactive)
      (setq-local async-shell-command-buffer 'confirm-kill-process)
      (if (projectile-project-p)
          (bpr-spawn (concat "upload_moai_game '" (projectile-project-root) "/lua'"))))

    (spacemacs/set-leader-keys-for-major-mode 'lua-mode "sm" 'moai-run-main)
    (spacemacs/set-leader-keys-for-major-mode 'lua-mode "su" 'moai-upload)))

(defun d12frosted-core/init-comment-dwim-2 ()
  (use-package comment-dwim-2
    :bind ("M-;" . comment-dwim-2)))

(defun d12frosted-core/post-init-elfeed ()
  (use-package elfeed
    :defer t
    :config
    (setq elfeed-feeds d12/elfeed-feeds)))

(defun d12frosted-core/post-init-elfeed-goodies ()
  (use-package elfeed-goodies
    :defer t
    :config
    (setq elfeed-goodies/powerline-default-separator 'utf-8)))

(defun d12frosted-core/init-bpr ()
    (use-package bpr
      :commands (bpr-spawn bpr-open-last-buffer)
      :config
      (setq bpr-colorize-output t)
      (setq bpr-close-after-success t)))

(defun d12frosted-core/post-init-zoom-frm ()
  (use-package zoom-frm
    :config
    ;; remove some crazy bindings
    (unbind-key "<C-wheel-down>")
    (unbind-key "<C-wheel-up>")))

(defun d12frosted-core/post-init-move-text ()
  (use-package move-text
    :config
    ;; remove some crazy bindings
    (bind-key "<M-down>" 'move-text-down prog-mode-map)
    (bind-key "<M-up>" 'move-text-up prog-mode-map)))

(defun d12frosted-core/post-init-mu4e ()
  (use-package move-text
    :init
    (setq mu4e-installation-path "/usr/local/Cellar/mu/0.9.16/share/emacs/site-lisp"
          mu4e-maildir "~/.mail"
          mu4e-drafts-folder "/[Gmail].Drafts"
          mu4e-sent-folder "/[Gmail].Sent Mail"
          mu4e-refile-folder "/[Gmail].All Mail"
          mu4e-trash-folder "/[Gmail].Trash"
          mu4e-get-mail-command "mbsync -a"
          mu4e-update-interval nil
          mu4e-compose-signature-auto-include nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t
          mu4e-sent-messages-behavior 'delete
          mu4e-maildir-shortcuts
          '(("/INBOX"               . ?i)
            ("/[Gmail].Sent Mail"   . ?s)
            ("/[Gmail].Trash"       . ?t)
            ("/[Gmail].All Mail"    . ?a))
          mu4e-enable-notifications t
          mu4e-enable-mode-line t)
    (with-eval-after-load 'mu4e-alert
      ;; Enable Desktop notifications
      (mu4e-alert-set-default-style 'notifier))
    (bind-key "<f5>" (lambda () (interactive ) (mu4e-update-mail-and-index t )))
    (require 'mu4e)))

(defun d12frosted-core/init-god-mode ()
  (use-package god-mode
    :commands (god-local-mode)
    :bind ("<escape>" . god-local-mode)
    :config
    (spacemacs|diminish god-local-mode)))

(defun d12frosted-core/init-composable ()
  (use-package composable
    :commands (composable-mode)
    :init
    (progn
      (composable-mode)
      (composable-def '(delete-region))
      (bind-key "C-M-w" 'composable-delete-region composable-mode-map))))

(defun d12frosted-core/init-counsel-app ()
  (use-package counsel-app
    :if (configuration-layer/layer-usedp 'spacemacs-ivy)
    :commands (counsel-app)
    :init
    (bind-key "M-<f12>" 'counsel-app)))

(defun d12frosted-core/init-flyspell-correct ()
  (use-package flyspell-correct
    :init
    (when (configuration-layer/layer-usedp 'spacemacs-ivy)
      (setq flyspell-correct-interface 'flyspell-correct/ivy))
    (when (configuration-layer/layer-usedp 'spacemacs-helm)
      (setq flyspell-correct-interface 'flyspell-correct/helm))
    (when (bound-and-true-p flyspell-correct-interface)
      (spacemacs/set-leader-keys "Sc" 'flyspell-correct/word))))

(defun d12frosted-core/init-electric-align ()
  (use-package electric-align)
  (add-hook 'prog-mode-hook 'electric-align-mode))

;; packages-config.el ends here
