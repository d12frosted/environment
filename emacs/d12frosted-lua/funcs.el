;;; funcs.el --- d12frosted-lua layer funcs file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(defun moai-run-main ()
  "Run main.lua using moai."
  (interactive)
  (let* ((buffer-name d12frosted-lua-moai-buffer-name)
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
          (async-shell-command (concat "cd " (shell-quote-argument workdir) "; moai main.lua") buffer buffer))
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

;;; funcs.el ends here
