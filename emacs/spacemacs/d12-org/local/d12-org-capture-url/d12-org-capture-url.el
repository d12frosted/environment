;;; d12-org-capture-url.el --- Parse and capture URL -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2017 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; Package-Version: 0.0.1
;; Package-Requires: ((ghub "1.1.0"))
;;
;; This file is not part of GNU Emacs.
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(require 'ghub)

(defconst d12-org-capture-github-issue-regex
  "\\(https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/issues/\\([[:digit:]]+\\)\\).*")

(defconst d12-org-capture-github-pull-regex
  "\\(https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/pull/\\([[:digit:]]+\\)\\).*")

(defconst d12-org-capture-github-commit-regex
  "\\(https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/commit/\\([[:alnum:]]+\\)\\).*")

(defconst d12-org-capture-github-commit-length 8)

(defun d12-org-capture-url (url)
  "Format url for capture template."
  (cond
   ((or (string-match d12-org-capture-github-issue-regex url)
        (string-match d12-org-capture-github-pull-regex url))
    (d12-org-capture-url--github-issue-or-pull
     (match-string 1 url)
     (match-string 2 url)
     (match-string 3 url)
     (match-string 4 url)))
   ((string-match d12-org-capture-github-commit-regex url)
    (d12-org-capture-url--github-commit
     (match-string 1 url)
     (match-string 2 url)
     (match-string 3 url)
     (match-string 4 url)))
   (t (format "%s" url))))

(defun d12-org-capture-url--github-issue-or-pull (url user repo issue)
  (let* ((api (format "/repos/%s/%s/issues/%s" user repo issue))
         (response (ghub-get api))
         (title (alist-get 'title response)))
    (format "[[%s][%s/%s#%s]] - %s"
            url user repo issue title)))

(defun d12-org-capture-url--github-commit (url user repo hash)
  (let* ((api (format "/repos/%s/%s/commits/%s" user repo hash))
         (response (ghub-get api))
         (commit (alist-get 'commit response))
         (message (alist-get 'message commit))
         (title (car (split-string message "\n")))
         (short-hash-length (min d12-org-capture-github-commit-length
                                 (length hash)))
         (short-hash (substring hash 0 short-hash-length)))
    (format "[[%s][%s/%s#%s]] - %s"
            url user repo short-hash title)))

(provide 'd12-org-capture-url)

;;; d12-org-capture-url.el ends here
