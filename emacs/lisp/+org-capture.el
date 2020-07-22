;;; +org-capture.el --- Org capture shortcuts -*- lexical-binding: t; -*-
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

;;;###autoload
(defun +org/capture-task ()
  "A short-cut for capturing todo task."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "t"))

;;;###autoload
(defun +org/capture-meeting ()
  "A short-cut for capturing meeting."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "m"))

;;;###autoload
(defun +org/capture-note ()
  "A short-cut for capturing note."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "n"))

;;;###autoload
(defun +org/capture-journal ()
  "A short-cut for capturing journal entry."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "j"))

(provide '+org-capture)
;;; +org-capture.el ends here
