;;; lang/vulpea/autoload/capture.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +capture-task ()
  "A short-cut for capturing a todo task."
  (interactive)
  (org-capture nil "t"))
