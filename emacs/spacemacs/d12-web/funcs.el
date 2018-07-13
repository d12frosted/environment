;;; funcs.el --- d12-web layer funcs file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:



;; https://emacs.stackexchange.com/a/566/5161
(defun d12-web/toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (setq d12-web-display-images
        (null d12-web-display-images))
  (d12-web/backup-display-property d12-web-display-images))

(defun d12-web/backup-display-property (invert &optional object)
  "Move the 'display property at POS to 'display-backup.
Only applies if display property is an image.
If INVERT is non-nil, move from 'display-backup to 'display
instead.
Optional OBJECT specifies the string or buffer. Nil means current
buffer."
  (let* ((inhibit-read-only t)
         (from (if invert 'display-backup 'display))
         (to (if invert 'display 'display-backup))
         (pos (point-min))
         left prop)
    (while (and pos (/= pos (point-max)))
      (if (get-text-property pos from object)
          (setq left pos)
        (setq left (next-single-property-change pos from object)))
      (if (or (null left) (= left (point-max)))
          (setq pos nil)
        (setq prop (get-text-property left from object))
        (setq pos (or (next-single-property-change left from object)
                      (point-max)))
        (when (eq (car prop) 'image)
          (add-text-properties left pos (list from nil to prop) object))))))



(defun d12-web/get-title (url)
  "Get title of a Web page by URL."
  (let ((raw-title (org-web-tools--html-title
                    (org-web-tools--get-url url))))
    (seq-reduce
     (lambda (r x)
       (replace-regexp-in-string x "" r))
     d12-web-title-substring-blacklist
     raw-title)))

;;; funcs.el ends here
