;;; lib-string.el --- String utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021, Boris Buliga <boris@d12frosted.io>
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
;; Utilities for `string' data type.
;;
;;; Code:

(require 'init-elpa)
(require 's)

;;;###autoload
(defun string-match-1 (regexp val)
  "Get the first group from REGEXP match of the VAL.

VAL can be either a string or a region (beg . end) of the
buffer."
  (string-match-n 1 regexp val))

;;;###autoload
(defun string-match-n (n regexp val)
  "Get the Nth group from REGEXP match of the VAL.

VAL can be either a string or a region (beg . end) of the
buffer."
  (let ((s (if (stringp val)
               val
             (buffer-substring (car val) (cdr val)))))
    (string-match regexp s)
    (match-string n s)))

;;;###autoload
(defun string-join-g (strs sep)
  "Join a list of STRS using SEP."
  (pcase strs
    (`nil "")
    (`(,str) str)
    (_ (mapconcat #'identity strs sep))))

;;;###autoload
(defun string-chop-prefix-regexp (prefix s)
  "Remove PREFIX regexp if it is at the start of S."
  (s-chop-prefix (car (s-match prefix s)) s))

;;;###autoload
(defun string-chop-suffix-regexp (suffix s)
  "Remove SUFFIX regexp if it is at the end of S."
  (s-chop-suffix (car (s-match suffix s)) s))

(defvar string-http-url-regexp
  "\\(https?://.*\\)"
  "HTTP(s) URL regexp.")

(defvar string-uuid-regexp
  (concat
   "\\("
   "[a-zA-Z0-9]\\{8\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{12\\}"
   "\\)")
  "UUID regexp.")

(defun string-from (value)
  "Convert VALUE to string."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((symbolp value) (symbol-name value))
   (t (user-error
       "Unsupported type of \"%s\"" value))))

(cl-defun string-table (&key data
                             pad-type
                             pad-str
                             sep
                             row-start
                             row-end)
  "Format DATA as a table.

DATA is list of lists. Each column is aligned by padding with
PAD-STR either on left or right depending on value of PAD-TYPE.

Each row begins with ROW-START and ends with ROW-END. Each value
in row is separated by SEP."
  (let* ((widths (seq-reduce
                  (lambda (r v)
                    (seq-map-indexed
                     (lambda (a i)
                       (max
                        (length (string-from a))
                        (or (nth i r)
                            0)))
                     v))
                  data
                  nil))
         (pad-fn (pcase (or pad-type 'left)
                   (`left #'s-pad-left)
                   (`right #'s-pad-right)))
         (pad-str (or pad-str " "))
         (row-start (or row-start ""))
         (row-end (or row-end ""))
         (sep (or sep " ")))
    (seq-reduce
     (lambda (r v)
       (concat
        r
        row-start
        (string-join
         (seq-map-indexed
          (lambda (a i)
            (funcall pad-fn
                     (nth i widths)
                     pad-str
                     (string-from a)))
          v)
         sep)
        row-end
        "\n"))
     data
     "")))

(provide 'lib-string)
;;; lib-string.el ends here
