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

(autoload 'org-map-entries "org")
(autoload 'org-id-get-create "org-id")

;;;###autoload
(defun +org-auto-id-add-to-headlines-in-file ()
  "Add ID property to the current file and all its headlines.

Only missing properties are added."
  (when (and (or (eq major-mode 'org-mode)
                 (eq major-mode 'org-journal-mode))
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (org-id-get-create)
      (org-map-entries #'org-id-get-create))))

(provide '+org-auto-id)
;;; +org-auto-id.el ends here
