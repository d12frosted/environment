;;; nucleus/autoload/files.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
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

;;;###autoload
(cl-defun nucleus-files-in
    (path-or-paths &rest rest
                   &key
                   filter
                   map
                   full
                   nosort
                   (follow-symlinks t)
                   (type 'files)
                   (relative-to (unless full default-directory))
                   (depth 99999)
                   (mindepth 0)
                   (match "/[^.]"))
  "Returns a list of files/directories in PATH-OR-PATHS (one string path or a
list of them).

FILTER is a function or symbol that takes one argument (the
path). If it returns non-nil, the entry will be excluded.

MAP is a function or symbol which will be used to transform each
entry in the results.

TYPE determines what kind of path will be included in the results. This can be t
(files and folders), 'files or 'dirs.

By default, this function returns paths relative to PATH-OR-PATHS
if it is a single path. If it a list of paths, this function
returns absolute paths. Otherwise, by setting RELATIVE-TO to a
path, the results will be transformed to be relative to it.

The search recurses up to DEPTH and no further. DEPTH is an
integer.

MATCH is a string regexp. Only entries that match it will be
included."
  (cond
   ((listp path-or-paths)
    (cl-loop for path in path-or-paths
             if (file-directory-p path)
             nconc (apply #'nucleus-files-in path (plist-put rest :relative-to relative-to))))
   ((let ((path path-or-paths)
          result)
      (when (file-directory-p path)
        (dolist (file (directory-files path nil "." nosort))
          (unless (member file '("." ".."))
            (let ((fullpath (expand-file-name file path)))
              (cond ((file-directory-p fullpath)
                     (when (and (memq type '(t dirs))
                                (string-match-p match fullpath)
                                (not (and filter (funcall filter fullpath)))
                                (not (and (file-symlink-p fullpath)
                                          (not follow-symlinks)))
                                (<= mindepth 0))
                       (setq result
                             (nconc result
                                    (list (cond (map (funcall map fullpath))
                                                (relative-to (file-relative-name fullpath relative-to))
                                                (fullpath))))))
                     (unless (< depth 1)
                       (setq result
                             (nconc result (apply #'nucleus-files-in fullpath
                                                  (append `(:mindepth ,(1- mindepth)
								      :depth ,(1- depth)
								      :relative-to ,relative-to)
                                                          rest))))))
                    ((and (memq type '(t files))
                          (string-match-p match fullpath)
                          (not (and filter (funcall filter fullpath)))
                          (<= mindepth 0))
                     (push (if relative-to
                               (file-relative-name fullpath relative-to)
                             fullpath)
                           result))))))
        result)))))
