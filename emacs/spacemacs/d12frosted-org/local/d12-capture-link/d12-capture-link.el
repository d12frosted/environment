;;; d12-capture-link.el --- smart capture link

;; Copyright (c) 2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 26 Jan 2017

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(defvar d12-capture-link-verbs-github
  '("Fix" "Merge" "Review" "Answer" "Close"))

(defvar d12-capture-link-verbs-generic
  '("Read" "Checkout"))

(defvar d12-capture-link-nouns-generic
  '("Article" "Post" "Message" "Project" "Library"))

(defun d12-capture-link (link)
  (interactive "MLink: ")
  (cond
   ((and (s-starts-with? "https://github.com/" link)
         (or (s-contains? "/issues/" link) (s-contains? "/pull/" link)))
    (d12-capture-link--github link))
   (t
    (d12-capture-link--generic link))))

(defun d12-capture-link--github (link)
  (let ((verb (completing-read "Choose verb: " d12-capture-link-verbs-github))
        short project url heading)
    (if (string-match "\\(https?://github\\.com/\\([a-zA-Z0-9\\-]+\\)/\\([a-zA-Z0-9\\-]+\\)/[a-zA-Z]+/\\([0-9]+\\)\\).*" link)
        (progn (setq short (format "%s/%s#%s"
                                   (match-string 2 link)
                                   (match-string 3 link)
                                   (match-string 4 link))
                     project (match-string 3 link)
                     url (match-string 1 link)
                     heading (format "%s: [[%s][%s]]" verb url short))
               (org-generate 'entry
                             (d12-org/get-file-path "d12frosted")
                             project
                             heading
                             (format "** TODO %s" heading)))
      (user-error "Could not capture GitHub link %s" link))))

(defun d12-capture-link--generic (link)
  (let ((verb (completing-read "Choose verb: " d12-capture-link-verbs-generic))
        (noun (completing-read "Choose noun: " d12-capture-link-nouns-generic)))
    (org-generate 'entry
                  (d12-org/get-file-path "inbox")
                  (format "TODO %s: [[%s][%s]]" verb link noun))))

(provide 'd12-capture-link)

;;; d12-capture-link.el ends here
