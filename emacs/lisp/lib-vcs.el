;;; lib-vcs.el --- VCS utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 08 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; GitHub URL parsing and formatting utilities for issues, pull requests,
;; milestones, commits, and project references.
;;
;;; Code:



(defvar vcs-url-github-issue-regexp
  (format "\\(%s/\\(%s\\)/\\(%s\\)/\\(%s\\)/\\(%s\\)\\).*"
          "https://github\\.com"
          "[-\\.[:alnum:]]+"
          "[-\\.[:alnum:]]+"
          "[[:alpha:]]+"
          "[[:alnum:]]+")
  "GitHub issue/pull URL regexp.

- 1st group is whole URL.
- 2nd group is owner.
- 3rd group is repository name.
- 4th group is type of the object - issue/pull/miliestone/commit.
- 5th group is the number of the object.")

(defvar vcs-url-github-project-regexp
  (format "\\(%s/\\(%s\\)/\\(%s\\)/?\\)"
          "https://github\\.com"
          "[-\\.[:alnum:]]+"
          "[-\\.[:alnum:]]+")
  "GitHub project URL regexp.

- 1st group is whole URL.
- 2nd group is owner.
- 3rd group is repository name.")

(defvar vcs-url-known-github-users '("d12frosted"
                                     "org-roam")
  "List of known GitHub users.")



(defun vcs-url-format-github-issue (owner repo type number)
  "Format GitHub issue.

Depending on the TYPE format can be one of the following:

- OWNER/REPO#NUMBER if TYPE is not milestone;
- OWNER/REPOmNUMBER otherwise.

In case OWNER is a member of `vcs-url-known-github-users', then OWNER
part of format is omitted."
  (concat
   (if (seq-contains-p vcs-url-known-github-users owner)
       repo
     (concat owner "/" repo))
   (if (string-equal type "milestone")
       "m"
     "#")
   (if (string-equal type "commit")
       (substring number 0 (min 7 (length number)))
     number)))

(defun vcs-url-format-github-project (owner repo)
  "Format GitHub REPO.

In case OWNER is a member of `vcs-url-known-github-users', then OWNER
part of format is omitted."
  (concat
   (if (seq-contains-p vcs-url-known-github-users owner)
       repo
     (concat owner "/" repo))))



(provide 'lib-vcs)
;;; lib-vcs.el ends here
