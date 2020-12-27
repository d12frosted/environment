;;; wine.el --- all things divine -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 13 Dec 2020
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

(require 'lib-fun)
(require '+org-notes)

(defvar org-roam-capture-immediate-template)
(autoload 'org-roam-find-file-immediate "org-roam")

;;; Regions and Appellations

(defun wine-region-select ()
  "Select a wine region or appellation."
  (+org-notes-select
   "Region"
   nil nil
   (lambda (entry)
     (let ((tags (plist-get (cdr entry) :tags)))
       (and (seq-contains-p tags "wine")
            (or (seq-contains-p tags "appellation")
                (seq-contains-p tags "region")))))))

;;; Grapes

(defvar wine-grape-template
  '("d" "default" plain
    #'org-roam-capture--get-point
    "%(wine--resources-template)%?"
    :file-name "wine/grape/%<%Y%m%d%H%M%S>-${slug}"
    :head "#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
    :unnarrowed t
    :immediate-finish t)
  "Capture template for grape entry.")

(defun wine-grape-select ()
  "Select a grape."
  (let ((result
         (+org-notes-select
          "Grape"
          nil nil
          (lambda (entry)
            (let ((tags (plist-get (cdr entry) :tags)))
              (and (seq-contains-p tags "wine")
                   (seq-contains-p tags "grape")))))))
    (if (plist-get result :id)
        result
      (let ((org-roam-capture-immediate-template wine-grape-template)
            (title (plist-get result :title)))
        (org-roam-find-file-immediate title nil nil t)
        (org-roam-db-build-cache)
        (seq-find
         (lambda (entry)
           (seq-contains-p (plist-get entry :tags) "grape"))
         (+org-notes-search title))))))

;;; Producers

(defun wine-producer-select ()
  "Select a grape."
  (+org-notes-select
   "Producer"
   nil nil
   (lambda (entry)
     (let ((tags (plist-get (cdr entry) :tags)))
       (and (seq-contains-p tags "wine")
            (seq-contains-p tags "producer"))))))

;;; Utilities

(defun wine--resources-template ()
  "Query for resource URL and return it as a meta string."
  (seq-reduce
   (lambda (r a)
     (concat r "- resources :: " a "\n"))
   (+repeat-fn
    (lambda ()
      (let ((url (read-string "URL: ")))
        (when (not (string-empty-p url))
          (org-link-make-string
           url
           (or (ignore-errors (url-domain (url-generic-parse-url url)))
               (read-string "Description: "))))))
    (lambda (a) (not (null a))))
   ""))

(provide 'wine)
;;; wine.el ends here
