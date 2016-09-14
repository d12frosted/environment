;;; funcs.el --- d12frosted-visual layer funcs file for Spacemacs.
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

(defun d12-visual/get-major-mode-icon-for-buffer ()
  "Return the icon for major-mode of current buffer."
  (let ((icon (all-the-icons-icon-for-buffer)))
    (condition-case nil
        (setq icon (all-the-icons-fileicon (file-name-extension (buffer-file-name (current-buffer)))))
      (error nil))
    icon))

(defun d12-visual/format-major-mode-icon (icon)
  "Format major-mode ICON.

ICON can be retrieved using
`d12-visual/get-major-mode-icon-for-buffer'."
  (if (symbolp icon)
      mode-name
    (format
     "%s"
     (propertize
      icon
      'help-echo (format "Major-mode: `%s`" major-mode)
      'display '(raise -0.1)
      'face `(:height 1.2
              :family ,(all-the-icons-icon-family-for-buffer)
              :background ,(if (powerline-selected-window-active)
                               (face-background 'powerline-active1)
                             (face-background 'powerline-inactive1)))))))

(defun d12-visual/powerline-major-mode ()
  "Return major-mode powerline segment."
  (if (display-graphic-p)
      (d12-visual/format-major-mode-icon
       (d12-visual/get-major-mode-icon-for-buffer))
    (powerline-major-mode)))

;;; funcs.el ends here
