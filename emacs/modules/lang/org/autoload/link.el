;;; lang/org/autoload/link.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Dec 2018
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
   "\\[" "\\(.*\\)" "\\]"

   ;; outer bracket
   "\\]"))

;;;###autoload
(defun +org-extract-id-from-link (link)
  (+string-match-n 2 +org-id-link-regexp link))
