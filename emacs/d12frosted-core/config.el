;;; config.el --- d12frosted-core layer config file for Spacemacs.
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

;; TODO: move me to d12frosted-org
(defvar d12-org/files-list
  '()
  "List of interesting org files.")

;; setup load path
(add-to-load-path (concat d12-path/emacs-layers "d12frosted-core/"))

;; require all modules
(require 'd12-files)
(require 'd12-helm)
(require 'd12-dir-settings)

;; load `private.el' file containing all sensitive data
(load (concat d12-path/emacs-private "private.el"))

;;; Setup variables

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
 ranger-override-dired t

 ;; Shell
 sh-basic-offset 2
 sh-indentation 2)

(setq projectile-enable-caching nil
      magit-repo-dirs '("~/Developer/")
      google-translate-default-source-language "Ukrainian"
      google-translate-default-target-language "English"
      git-messenger:show-detail t
      haskell-process-suggest-remove-import-lines nil
      haskell-process-suggest-hoogle-imports nil
      powerline-default-separator 'utf-8)

(spacemacs|use-package-add-hook helm
  :post-config
  ;; Disable fuzzy matching to make mdfind work with helm-locate
  (setq helm-locate-fuzzy-match nil)
  (setq helm-locate-command "mdfind -name %s %s")
  (if (configuration-layer/layer-usedp 'spacemacs-helm)
      (progn
        (helm-projectile-on)
        (bind-key "C-s" 'helm-swoop)
        (bind-key "C-S-s" 'spacemacs/helm-swoop-region-or-symbol))))

(spacemacs|use-package-add-hook ivy
  :post-config
  (if (configuration-layer/layer-usedp 'spacemacs-ivy)
      (bind-key "C-S-s" 'spacemacs/swiper-region-or-symbol)))

;;; Auto modes

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;; Hooks

(add-hook 'find-file-hook 'd12-dir-settings/load)
(add-hook 'company-mode-hook 'company-quickhelp-mode)
(add-hook 'prog-mode-hook 'vimish-fold-mode)
(add-hook 'haskell-interactive-mode-hook 'd12//init-haskell-interactive-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'after-save-hook 'delete-trailing-whitespace)

;;; Configs

(delete-selection-mode 1)

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

;; python
(defun pyenv-mode-versions ()
  "List installed python versions."
  (let ((versions (shell-command-to-string "vf ls")))
    (delete-dups (cons "system" (split-string versions)))))

;; key bindings
(evil-leader/set-key
  "it" 'd12/insert-time
  "id" 'd12/insert-date
  "iD" 'd12/insert-full-date
  "p#" 'projectile-replace-regexp)

;; MOAI
(defun moai-run-main ()
  "Run main.lua using moai."
  (interactive)
  (setq-local async-shell-command-buffer 'confirm-kill-process)
  (if (projectile-project-p)
      (let ((root (projectile-project-root)))
        (projectile-save-project-buffers)
        (async-shell-command (concat "cd '" root "'; and moai main.lua")))
    (async-shell-command (concat "moai " (buffer-file-name)))))
(defun moai-run-main-multiplayer ()
  "Run main.lua using moai in multiplayer mode."
  (interactive)
  (setq-local async-shell-command-buffer 'confirm-kill-process)
  (if (projectile-project-p)
      (let ((root (projectile-project-root)))
        (projectile-save-project-buffers)
        (async-shell-command (concat "cd '" root "'; and moai main.lua")))
    (async-shell-command (concat "moai " (buffer-file-name) " -r 5152") "moai-server")
    (async-shell-command (concat "moai " (buffer-file-name) " -c 127.0.0.1 5152") "moai-client")))
(defun moai-upload ()
  "Upload moai game to device."
  (interactive)
  (setq-local async-shell-command-buffer 'confirm-kill-process)
  (if (projectile-project-p)
      (shell-command (concat "upload_moai_game '" (projectile-project-root) "'"))))

(spacemacs/set-leader-keys-for-major-mode 'lua-mode "sm" 'moai-run-main)
(spacemacs/set-leader-keys-for-major-mode 'lua-mode "su" 'moai-upload)

;; reset key bindings
(unbind-key "<C-wheel-down>")
(unbind-key "<C-wheel-up>")

;;; config.el ends here
