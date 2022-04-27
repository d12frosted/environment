;;; lib-directory.el --- Directory utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 14 Feb 2021
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
;; Various utilities for files and directories.
;;
;;; Code:

(defun directory-subdirs (directory &optional rec)
  "Return subdirs or files of DIRECTORY.

If REC is non-nil then do recursive search."
  (let ((res
         (seq-map
          #'file-name-as-directory
          (seq-remove
           (lambda (file)
             (or (string-match "\\`\\."
                               (file-name-nondirectory file))
                 (string-match "\\`#.*#\\'"
                               (file-name-nondirectory file))
                 (string-match "~\\'"
                               (file-name-nondirectory file))
                 (not (file-directory-p file))))
           (directory-files directory t)))))
    (if rec
        (apply
         #'append
         (seq-map (lambda (p) (cons p (directory-subdirs p)))
                  res))
      res)))

(provide 'lib-directory)
;;; lib-directory.el ends here
