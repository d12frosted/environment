;;; ui/dashboard/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +nucleus-dashboard/open (frame)
  "Switch to the dashboard in the current window, of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (+nucleus-dashboard-initial-buffer))
    (+nucleus-dashboard-reload t)))

;;;###autoload
(defun +nucleus-dashboard/forward-button (n)
  "Like `forward-button', but don't wrap."
  (interactive "p")
  (forward-button n nil))

;;;###autoload
(defun +nucleus-dashboard/backward-button (n)
  "Like `backward-button', but don't wrap."
  (interactive "p")
  (backward-button n nil))
