;;; init-file.el --- File utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 14 Feb 2020
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

(defalias '+file-locate-dominting-file #'locate-dominating-file)

(defun +file-locate-dominting-dir (file name)
  "Starting at FILE, look up directory hierarchy for directory with NAME.
FILE can be a file or a directory. If it's a file, its directory
will serve as the starting point for searching the hierarchy of
directories. Stop at the first parent directory with NAME, and
return the directory. Return nil if not found."
  (expand-file-name
   (locate-dominating-file
    buffer-file-name
    (lambda (dir)
      (and (file-directory-p dir)
           (string-equal name
                         (file-name-nondirectory
                          (directory-file-name
                           (file-name-directory dir)))))))))

(defun +file-subdirs (directory &optional filep)
  "Return subdirs or files of DIRECTORY according to FILEP."
  (cl-remove-if
   (lambda (file)
     (or (string-match "\\`\\."
                       (file-name-nondirectory file))
         (string-match "\\`#.*#\\'"
                       (file-name-nondirectory file))
         (string-match "~\\'"
                       (file-name-nondirectory file))
         (if filep
             (file-directory-p file)
           (not (file-directory-p file)))))
   (directory-files directory t)))

(provide 'init-file)
;;; init-file.el ends here
