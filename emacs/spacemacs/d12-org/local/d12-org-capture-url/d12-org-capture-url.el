;;; d12-org-capture-url.el --- Parse and capture URL -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2017 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; Package-Version: 0.0.1
;; Package-Requires: ((gh "0.10.0"))
;;
;; This file is not part of GNU Emacs.
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(require 'gh-issues)

(defconst d12-org-capture-github-issue-regex
  "\\(https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/[[:alpha:]]+/\\([[:digit:]]+\\)\\).*")

(defun d12-org-capture-url (url)
  "Format url for capture template."
  (cond
   ((string-match d12-org-capture-github-issue-regex url)
    (d12-org-capture--github-issue
     (match-string 1 url)
     (match-string 2 url)
     (match-string 3 url)
     (match-string 4 url)))
   (t (format "%s" url))))

(defun d12-org-capture--github-issue (url user repo issue)
  (let* ((api (gh-issues-api "api" :sync nil :cache nil :num-retries 1))
         (response (gh-issues-issue-get api user repo issue))
         (title (oref (oref response data) title)))
    (format "[[%s][%s/%s#%s]] - %s"
            url user repo issue title)))

(provide 'd12-org-capture-url)

;;; d12-org-capture-url.el ends here
