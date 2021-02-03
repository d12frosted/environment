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

(defun +org/capture-task ()
  "A short-cut for capturing todo task."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "t"))

(defun +org/capture-meeting ()
  "A short-cut for capturing meeting."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "m"))

(defun +org/capture-note ()
  "A short-cut for capturing note."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "n"))

(defun +org/capture-journal ()
  "A short-cut for capturing journal entry."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "j"))

(defun +capture-meeting-target ()
  "Return a target for a meeting capture."
  (let ((person (org-capture-get :meeting-person)))
    ;; unfortunately, I could not find a way to reuse
    ;; `org-capture-set-target-location'
    (if (vulpea-note-id person)
        (let ((path (vulpea-note-path person))
              (headline "Meetings"))
          (set-buffer (org-capture-target-buffer path))
	        ;; Org expects the target file to be in Org mode, otherwise
	        ;; it throws an error.  However, the default notes files
	        ;; should work out of the box.  In this case, we switch it to
	        ;; Org mode.
	        (unless (derived-mode-p 'org-mode)
	          (org-display-warning
	           (format "Capture requirement: switching buffer %S to Org mode"
		                 (current-buffer)))
	          (org-mode))
	        (org-capture-put-target-region-and-position)
	        (widen)
	        (goto-char (point-min))
	        (if (re-search-forward (format org-complex-heading-regexp-format
					                               (regexp-quote headline))
				                         nil t)
	            (beginning-of-line)
	          (goto-char (point-max))
	          (unless (bolp) (insert "\n"))
	          (insert "* " headline "\n")
	          (beginning-of-line 0)))
      (let ((path +capture-inbox-file))
        (set-buffer (org-capture-target-buffer path))
	      (org-capture-put-target-region-and-position)
	      (widen)))))

(defun +capture-meeting-template ()
  "Return a template for a meeting capture."
  (let ((person (vulpea-select
                 "Person"
                 nil nil
                 (lambda (note)
                   (let ((tags (vulpea-note-tags note)))
                     (seq-contains-p tags "people"))))))
    (org-capture-put :meeting-person person)
    (if (vulpea-note-id person)
        "* MEETING [%<%Y-%m-%d %a>] :MEETING:REFILE:\n%U\n\n%?"
      (concat "* MEETING with "
              (vulpea-note-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

(provide '+org-capture)
;;; +org-capture.el ends here
