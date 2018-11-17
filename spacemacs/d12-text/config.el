;;; config.el --- d12-text layer config file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defvar d12-text-http-regexp
  "\\(https?://.*\\)")
(defvar d12-text-github-regexp
  "\\(https?://github\\.com/\\([-[:alnum:]]+\\)/\\([-[:alnum:]]+\\)/\\([[:alpha:]]+\\)/\\([[:digit:]]+\\)\\).*")
(defvar d12-text-jira-regexps
  '("\\(https?://jira.[-[:alnum:]]+\\.[[:alpha:]]+/browse/\\([-[:alnum:]]+\\).*\\)"
    "\\(https?://jira.[-[:alnum:]]+\\.[[:alpha:]]+/.*selectedIssue=\\([-[:alnum:]]+\\).*\\)"
    "\\(https?://jira.[-[:alnum:]]+\\.[[:alpha:]]+/projects/[[:alpha:]]+/issues/\\([-[:alnum:]]+\\).*\\)"))
(defvar d12-text-github-user "d12frosted")
;; for private.el usage
(defvar d12-text-known-github-owners '())

;;; config.el ends here
