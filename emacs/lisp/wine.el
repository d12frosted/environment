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

(require '+org-notes)

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

(defun wine-grape-select ()
  "Select a grape."
  (+org-notes-select
   "Grape"
   nil nil
   (lambda (entry)
     (let ((tags (plist-get (cdr entry) :tags)))
       (and (seq-contains-p tags "wine")
            (seq-contains-p tags "grape"))))))

(defun wine-producer-select ()
  "Select a grape."
  (+org-notes-select
   nil nil
   (lambda (entry)
     (let ((tags (plist-get (cdr entry) :tags)))
       (and (seq-contains-p tags "wine")
            (seq-contains-p tags "producer"))))))

(provide 'wine)
;;; wine.el ends here
