;;; +capture.el --- Org capture shortcuts -*- lexical-binding: t; -*-
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

(require 'init-env)
;; (require 'org-capture)
;; (require 'vulpea)

(defvar +capture-inbox-file (format "inbox-%s.org" +sys-name)
  "The path to the inbox file.

It is relative to `org-directory', unless it is absolute.")

(defun +capture-task ()
  "Capture a task."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "t"))

(defun +capture-meeting ()
  "Capture a meeting."
  (interactive)
  (message "starting capture ...")
  (org-capture nil "m"))

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
                 :filter-fn
                 (lambda (note)
                   (let ((tags (vulpea-note-tags note)))
                     (seq-contains-p tags "people"))))))
    (org-capture-put :meeting-person person)
    (if (vulpea-note-id person)
        "* MEETING [%<%Y-%m-%d %a>] :MEETING:REFILE:\n%U\n\n%?"
      (concat "* MEETING with "
              (vulpea-note-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

(defvar +capture-person-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "people/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n"
            "\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for person.

Variables in the capture context are provided by
`vulpea-create'.")

(defvar +capture-article-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "litnotes/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n"
            "#+TIME-STAMP: <>\n"
            "#+ROAM_KEY: ${url}\n"
            "#+ROAM_TAGS: Content:Article Status:New\n"
            "\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for article.

Variables in the capture context are provided by
`+capture-article'.")

(defun +capture-article ()
  "Capture an article using `+capture-article-template'.

User is asked to provide an URL, title and authors of the article
being captured.

Title is inferred from URL, but user may edit it.

Authors can be created on the fly. See `+capture-person-template'
for more information."
  (interactive)
  (when-let* ((url (read-string "URL: "))
              (title (org-cliplink-retrieve-title-synchronously url))
              (title (read-string "Title: " title))
              (authors (+fun-collect-while
                        (lambda ()
                          (let ((person
                                 (vulpea-select
                                  "Person"
                                  :filter-fn
                                  (lambda (note)
                                    (let ((tags (vulpea-note-tags note)))
                                      (seq-contains-p tags "people"))))))
                            (if (vulpea-note-id person)
                                person
                              (vulpea-create (vulpea-note-title person)
                                             +capture-person-template))))
                        nil))
              (note (vulpea-create title
                                   +capture-article-template
                                   (list (cons 'url url)))))
    (vulpea-utils-with-note note
      (vulpea-meta-set note "authors" authors t))
    (find-file (vulpea-note-path note))))

(provide '+capture)
;;; +capture.el ends here
