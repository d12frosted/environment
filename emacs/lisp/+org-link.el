;;; +org-link.el --- link utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Nov 2019
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

(require '+string)

(declare-function org-link-make-string "ol")

;;;###autoload
(defvar +org-id-link-regexp
  (concat
   ;; outer bracket
   "\\["

   ;; link
   "\\["
   ;; type
   "\\([[a-zA-Z0-9\\-]+\\)"
   ":"
   ;; uuid
   "\\("
   "[a-zA-Z0-9]\\{8\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{12\\}"
   "\\)"
   "\\]"

   ;; title
   "\\[" "\\(.*?\\)" "\\]"

   ;; outer bracket
   "\\]"))

;;;###autoload
(defun +org-extract-id-from-link (link)
  "Extract headline id from LINK."
  (+string-match-n 2 +org-id-link-regexp link))

(defun +org-insert-url (url)
  "Insert org link for URL.

If it's a link to Web page, then the domain is used as
description."
  (interactive "sURL: ")
  (insert (org-link-make-string
           url
           (or (ignore-errors (url-domain (url-generic-parse-url url)))
               (read-string "Description: ")))))

(provide '+org-link)
;;; +org-link.el ends here
