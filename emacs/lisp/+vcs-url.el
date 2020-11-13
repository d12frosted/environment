;;; +vcs-url.el --- dealing with VCS URLs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 11 Oct 2020
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

(defvar +vcs-url-github-repo-regexp
  "\\(https?://github\\.com/\\([-[:alnum:]]+\\)/\\([-[:alnum:]]+\\)\\).*"
  "GitHub repo URL regexp.

- 1st group is whole URL.
- 2nd group is owner.
- 3rd group is repository name.")

(defvar +vcs-url-github-issue-regexp
  "\\(https://github\\.com/\\([-[:alnum:]]+\\)/\\([-[:alnum:]]+\\)/\\([[:alpha:]]+\\)/\\([[:alnum:]]+\\)\\).*"
  "GitHub issue/pull URL regexp.

- 1st group is whole URL.
- 2nd group is owner.
- 3rd group is repository name.
- 4th group is type of the object - issue/pull/miliestone/commit.
- 5th group is the number of the object.")

(defvar +vcs-url-github-project-regexp
  "\\(https://github\\.com/\\([-[:alnum:]]+\\)/\\([-[:alnum:]]+\\)/?\\)"
  "GitHub project URL regexp.

- 1st group is whole URL.
- 2nd group is owner.
- 3rd group is repository name.")

(defvar +vcs-url-known-github-users '("d12frosted")
  "List of 'known' GitHub users.")

(defun +vcs-url-format-github-issue (owner repo type number)
  "Format GitHub issue.

Depending on the TYPE format can be one of the following:

- OWNER/REPO#NUMBER if TYPE is not milestone;
- OWNER/REPOmNUMBER otherwise.

In case OWNER is a member of `+vcs-url-known-github-users', then OWNER
part of format is omitted."
  (concat
   (if (seq-contains-p +vcs-url-known-github-users owner)
       repo
     (concat owner "/" repo))
   (if (string-equal type "milestone")
       "m"
     "#")
   (if (string-equal type "commit")
       (substring number 0 (min 7 (length number)))
     number)))

(defun +vcs-url-format-github-project (owner repo)
  "Format GitHub REPO.

In case OWNER is a member of `+vcs-url-known-github-users', then OWNER
part of format is omitted."
  (concat
   (if (seq-contains-p +vcs-url-known-github-users owner)
       repo
     (concat owner "/" repo))))

(provide '+vcs-url)
;;; +vcs-url.el ends here
