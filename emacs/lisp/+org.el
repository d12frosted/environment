;;; +org.el --- general purpose utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Nov 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(autoload 'org-up-heading-safe "org")
(autoload 'org-remove-empty-drawer-at "org")
(autoload 'org-id-get-create "org-id")

(defmacro +org-with-file (file &rest body)
  "Execute BODY in `org-mode' FILE."
  (declare (indent 1) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     ,@body))

(defun +org-parent-id ()
  "Return parent id of entry at point."
  (save-excursion
    (when (org-up-heading-safe)
      (org-id-get-create))))

(defun +org/remove-empty-drawer ()
  "Remove empty drawer at point."
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(provide '+org)
;;; +org.el ends here
