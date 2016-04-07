;;; packages.el --- d12frosted-core layer packages file for Spacemacs.
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

(defconst d12frosted-core-packages
  '(beacon
    ranger
    google-translate
    projectile
    spaceline
    magit
    git-messenger
    helm
    ivy
    glsl-mode
    lua-mode
    comment-dwim-2
    elfeed
    elfeed-goodies
    bpr
    zoom-frm
    move-text
    mu4e))

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
    (helm-projectile-on)))

(defun d12frosted-core/post-init-spaceline ()
  (use-package spaceline-config
    :init
    (setq powerline-default-separator 'utf-8)))

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

;; packages.el ends here
