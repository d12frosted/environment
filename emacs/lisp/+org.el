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

(eval-when-compile
  (require 'org)
  (require 'org-agenda)
  (require 'org-habit)
  (require 'outline)
  (require 'time-date)
  (declare-function org-id-get-create "org")
  (declare-function org-back-to-heading "org")
  (declare-function org-up-heading-safe "org")
  (declare-function org-heading-components "org")
  (declare-function org-end-of-subtree "org")
  (declare-function org-get-todo-state "org")
  (declare-function org-get-tags "org")
  (declare-function org-time-string-to-time "org")
  (declare-function org-entry-get "org")
  (declare-function org-agenda-redo "org-agenda")
  (declare-function org-is-habit-p "org-habit")
  (declare-function outline-next-heading "outline")
  (declare-function time-to-seconds "time-date"))

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

(provide '+org)
;;; +org.el ends here
