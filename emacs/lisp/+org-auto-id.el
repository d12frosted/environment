;;; +org-auto-id.el --- automatically set header id -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
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

(require 'org-id)

(defvar +org-auto-id-props '("ID" "CUSTOM_ID")
  "List of properties that must be added when missing.")

(defconst +org-auto-id-enable-regexp
  "^#\\+OPTIONS:.*auto-id:t")

;;;###autoload
(defun +org-auto-id-dwim ()
  "Just do the dirty job for the heading at point."
  (mapc (lambda (prop)
          (+org-auto-id-get (point) prop 'create))
        +org-auto-id-props))

;;;###autoload
(defun +org-auto-id-get (&optional pom prop create)
  "Get the PROP of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point. If the entry does not
have a PROP, the function returns nil. However, when CREATE is
non nil, create a PROP if none is present already. In any case,
the PROP of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil prop)))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new))
        (org-entry-put pom prop id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

;;;###autoload
(defun +org-auto-id-add-to-headlines-in-file ()
  "Add id properties to all headlines in the current file.

Only missing properties are added.

Only adds ids if the `auto-id' option is set to t in the file
somewhere. For example,

  #+OPTIONS: auto-id:t"
  (when (and (or (eq major-mode 'org-mode)
                 (eq major-mode 'org-journal-mode))
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward +org-auto-id-enable-regexp (point-max) t)
        (org-map-entries #'+org-auto-id-dwim)))))

(provide '+org-auto-id)
;;; +org-auto-id.el ends here
