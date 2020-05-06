;;; lang/vulpea/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +notes-setup-buffer (&optional _)
  "Setup current buffer for notes viewing and editing.

If the current buffer is not a note, does nothing."
  (interactive)
  (unless (active-minibuffer-window)
    (if (and buffer-file-name
             (string-equal notes-directory
                           (file-name-directory buffer-file-name)))
        (progn
          (unless (bound-and-true-p org-roam-mode)
            (org-roam-mode 1))
          (setq-local time-stamp-start "#\\+TIME-STAMP:[ 	]+\\\\?[\"<]+")
          (setq org-roam-last-window (get-buffer-window))
          (unless (eq 'visible (org-roam--current-visibility))
            (delete-other-windows)
            (call-interactively #'org-roam)))
      (when (and (fboundp #'org-roam-buffer--visibility)
                 (eq 'visible (org-roam--current-visibility)))
        (delete-window (get-buffer-window org-roam-buffer))))))

;;;###autoload
(defun +notes-rebuild ()
  "Rebuild notes database."
  (interactive)
  (org-roam-db--clear)
  (org-roam-db-build-cache))
