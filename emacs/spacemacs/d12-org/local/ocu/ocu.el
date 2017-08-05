;;; ocu.el --- Parse and capture URL -*- lexical-binding: t; -*-
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

(defconst ocu-github-issue-regex
  "\\(https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/issues/\\([[:digit:]]+\\)\\).*")

(defconst ocu-github-pull-regex
  "\\(https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/pull/\\([[:digit:]]+\\)\\).*")

(defconst ocu-github-commit-regex
  "\\(https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/commit/\\([[:alnum:]]+\\)\\).*")

(defconst ocu-github-commit-length 8)

(defconst ocu-assembla-issue-regex
  "https://app.assembla.com/spaces/\\([[:alnum:]\-]+\\)/tickets/.*ticket=\\([[:digit:]]+\\)")

(defun ocu (url)
  "Format url for capture template."
  (cond
   ((or (string-match ocu-github-issue-regex url)
        (string-match ocu-github-pull-regex url))
    (ocu--github-issue-or-pull
     (match-string 1 url)
     (match-string 2 url)
     (match-string 3 url)
     (match-string 4 url)))
   ((string-match ocu-github-commit-regex url)
    (ocu--github-commit
     (match-string 1 url)
     (match-string 2 url)
     (match-string 3 url)
     (match-string 4 url)))
   (t (format "%s" url))))

(defun ocu--github-issue-or-pull (url user repo issue)
  (let* ((api (format "/repos/%s/%s/issues/%s" user repo issue))
         (response (ghub-get api))
         (title (alist-get 'title response)))
    (format "[[%s][%s/%s#%s]] - %s"
            url user repo issue title)))

(defun ocu--github-commit (url user repo hash)
  (let* ((api (format "/repos/%s/%s/commits/%s" user repo hash))
         (response (ghub-get api))
         (commit (alist-get 'commit response))
         (message (alist-get 'message commit))
         (title (car (split-string message "\n")))
         (short-hash-length (min ocu-github-commit-length
                                 (length hash)))
         (short-hash (substring hash 0 short-hash-length)))
    (format "[[%s][%s/%s#%s]] - %s"
            url user repo short-hash title)))

(provide 'ocu)

;;; ocu.el ends here
