;;; core/autoload/cli.el -*- lexical-binding: t; -*-

(require 'core-cli)

(defun nucleus--run (command &optional yes)
  (let ((default-directory nucleus-emacs-dir)
        (nucleus-auto-accept yes))
    (let ((compilation-buffer-name-function (lambda (_) "*bin/nucleus*")))
      (compile (format "bin/nucleus %s" command) t))
    (while compilation-in-progress
      (sit-for 1))
    (when (y-or-n-p "Reload Doom config?")
      (nucleus/reload))
    (message "Done")))


;;;###autoload
(defun nucleus//update (&optional yes)
  "TODO"
  (interactive "P")
  (nucleus--run "update" yes))

;;;###autoload
(defun nucleus//upgrade (&optional yes)
  "TODO"
  (interactive "P")
  (nucleus--run "upgrade" yes))

;;;###autoload
(defun nucleus//install (&optional yes)
  "TODO"
  (interactive "P")
  (nucleus--run "install" yes))

;;;###autoload
(defun nucleus//autoremove (&optional yes)
  "TODO"
  (interactive "P")
  (nucleus--run "autoremove" yes))

;;;###autoload
(defun nucleus//refresh (&optional yes)
  "TODO"
  (interactive "P")
  (nucleus--run "refresh" yes))

;;;###autoload
(defun nucleus/reload (&optional force-p)
  "Reloads your config. This is experimental!

If called from a noninteractive session, this will try to communicate with a
live server (if one is found) to tell it to run this function.

If called from an interactive session, tries to reload autoloads files (if
necessary), reinistalize nucleus (via `nucleus-initialize') and reloads your private
init.el and config.el. Then runs `nucleus-reload-hook'."
  (interactive "P")
  (require 'core-cli)
  (nucleus-reload-autoloads force-p)
  (setq load-path nucleus-site-load-path)
  (let (nucleus-init-p)
    (nucleus-initialize))
  (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
    (nucleus-initialize-modules 'force))
  (run-hook-wrapped 'nucleus-reload-hook #'nucleus-try-run-hook)
  (message "Finished!"))
