;;; lang/org/+modeline.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Nov 2018
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

(after! org-clock
  (defun org-clock-get-clock-string ()
    "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
    (let ((clocked-time (org-clock-get-clocked-time)))
      (if org-clock-effort
	        (let* ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
	               (work-done-str
		              (propertize (org-duration-from-minutes clocked-time)
			                        'face
			                        (if (and org-clock-task-overrun
				                               (not org-clock-task-overrun-text))
				                          'org-mode-line-clock-overrun
			                          'org-mode-line-clock)))
	               (effort-str (org-duration-from-minutes effort-in-minutes)))
	          (format (propertize " [%s/%s] " 'face 'org-mode-line-clock)
		                work-done-str effort-str))
        (format (propertize " [%s] " 'face 'org-mode-line-clock)
	              (org-duration-from-minutes clocked-time)))))

  (defun org-clock-update-mode-line (&optional refresh)
    "Update mode line with clock information.
When optional argument is non-nil, refresh cached heading."
    (if org-clock-effort
        (org-clock-notify-once-if-expired)
      (setq org-clock-task-overrun nil))
    (when refresh (setq org-clock-heading (org-clock--mode-line-heading)))
    (setq org-mode-line-string
	        (propertize
	         (let ((clock-string (org-clock-get-clock-string))
	               (help-text org-clock-heading))
	           (if (and (> org-clock-string-limit 0)
		                  (> (length clock-string) org-clock-string-limit))
	               (propertize
		              (substring clock-string 0 org-clock-string-limit)
		              'help-echo (concat help-text ": " org-clock-heading))
	             (propertize clock-string 'help-echo help-text)))
	         'local-map org-clock-mode-line-map
	         'mouse-face 'mode-line-highlight))
    (if (and org-clock-task-overrun org-clock-task-overrun-text)
        (setq org-mode-line-string
	            (concat (propertize
		                   org-clock-task-overrun-text
		                   'face 'org-mode-line-clock-overrun) org-mode-line-string)))
    (force-mode-line-update)))
