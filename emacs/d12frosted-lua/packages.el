;;; packages.el --- d12frosted-lua layer packages file for Spacemacs.
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

(defconst d12frosted-lua-packages
  '(lua-mode
    smart-ops))

(defun d12frosted-lua/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (defun moai-run-main ()
      "Run main.lua using moai."
      (interactive)
      (let* ((buffer-name "*moai main.lua*")
             (buffer (get-buffer buffer-name))
             (currentBuffer (current-buffer))
             workdir)
        (when buffer
          (when-let ((window (get-buffer-window buffer)))
            (delete-window window))
          (kill-buffer buffer))
        (setq buffer (generate-new-buffer buffer-name))
        (if (projectile-project-p)
            (let ((root (projectile-project-root)))
              (projectile-save-project-buffers)
              (setq workdir (concat root "lua"))
              (async-shell-command (concat "cd " (shell-quote-argument workdir) "; and moai main.lua") buffer buffer))
          (async-shell-command (concat "moai " (buffer-file-name)) buffer buffer))
        (switch-to-buffer-other-window buffer)
        (when workdir
          (setq-local default-directory workdir))
        (select-window (get-buffer-window currentBuffer))))

    (defun moai-upload ()
      "Upload moai game to device."
      (interactive)
      (setq-local async-shell-command-buffer 'confirm-kill-process)
      (if (projectile-project-p)
          (bpr-spawn (concat "upload_moai_game '" (projectile-project-root) "/lua'"))))

    (spacemacs/set-leader-keys-for-major-mode 'lua-mode "sm" 'moai-run-main)
    (spacemacs/set-leader-keys-for-major-mode 'lua-mode "su" 'moai-upload)))

(defun d12frosted-lua/post-init-smart-ops ()
  (use-package smart-ops
    :after lua-mode
    :config
    (define-smart-ops-for-mode 'lua-mode
      (smart-ops "," :pad-before nil)
      (smart-ops ":")
      (smart-ops-default-ops))))


;;; packages.el ends here
