;;; lang/org/autoload/capture.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
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
  (org-capture nil "t"))

;;;###autoload
(defun +org/capture-meeting ()
  "A short-cut for capturing meeting."
  (interactive)
  (org-capture nil "m"))

;;;###autoload
(defun +org/capture-note ()
  "A short-cut for capturing note."
  (interactive)
  (org-capture nil "n"))

;;;###autoload
(defun +org/capture-journal ()
  "A short-cut for capturing journal entry."
  (interactive)
  (org-capture nil "j"))
