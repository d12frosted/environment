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

(defun wine-region-select ()
  "Select a wine region or appellation."
  (interactive)
  (when-let* ((res
               (+org-notes-select
                nil nil
                (lambda (entry)
                  (let ((tags (plist-get (cdr entry) :tags)))
                    (and (seq-contains-p tags "wine")
                         (or (seq-contains-p tags "appellation")
                             (seq-contains-p tags "region")))))))
              (id (+seq-singleton
                   (+seq-flatten
                    (+org-notes-get-file-id (plist-get res :path))))))
    (plist-put res :id id)))

(provide 'wine)
;;; wine.el ends here
