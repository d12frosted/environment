;;; lang/vulpea/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro +org-with-file (file &rest body)
  "Execute BODY in `org-mode' FILE."
  `(with-current-buffer (find-file-noselect ,file)
     ,@body))

;;;###autoload
(defun +org-parent-id ()
  "Return parent id of entry at point."
  (save-excursion
    (when (org-up-heading-safe)
      (org-id-get-create))))

;;;###autoload
(defun +org/remove-empty-drawer ()
  "Remove empty drawer at point."
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))
