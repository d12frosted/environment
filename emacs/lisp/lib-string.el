;;; lib-string.el --- String utilities -*- lexical-binding: t; -*-
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
;; Utilities for `string' data type.
;;
;;; Code:

(require 'init-vulpea)

(autoload 'vulpea-note-p "vulpea-note")

(defun string-non-empty-p (str)
  "Return non-nil when STR is non-empty."
  (not (string-empty-p str)))

(defun string-match-1 (regexp val)
  "Get the first group from REGEXP match of the VAL.

VAL can be either a string or a region (beg . end) of the
buffer."
  (string-match-n 1 regexp val))

(defun string-match-n (n regexp val)
  "Get the Nth group from REGEXP match of the VAL.

VAL can be either a string or a region (beg . end) of the
buffer."
  (let ((s (if (stringp val)
               val
             (buffer-substring (car val) (cdr val)))))
    (string-match regexp s)
    (match-string n s)))

(defun string-join-g (strs sep)
  "Join a list of STRS using SEP."
  (pcase strs
    (`nil "")
    (`(,str) str)
    (_ (mapconcat #'identity strs sep))))

(defun string-chop-prefix-regexp (prefix s)
  "Remove PREFIX regexp if it is at the start of S."
  (s-chop-prefix (car (s-match prefix s)) s))

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

 (defun string-group-number (num &optional size char)
   "Format NUM as string grouped to SIZE with CHAR."
   ;; Based on code for `math-group-float' in calc-ext.el
   (let* ((size (or size 3))
          (char (or char " "))
          (str (if (stringp num)
                   num
                 (number-to-string num)))
          ;; omitting any trailing non-digit chars
          ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
          (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
     (while (> pt size)
       (setq str (concat (substring str 0 (- pt size))
                         char
                         (substring str (- pt size)))
             pt (- pt size)))
     str))

(defun string-from (value)
  "Convert VALUE to string."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((symbolp value) (symbol-name value))
   ((vulpea-note-p value) (vulpea-buttonize value))
   (t (user-error
       "Unsupported type of \"%s\"" value))))

(cl-defun string-from-number (num
                              &key
                              min-length
                              padding
                              padding-num)
  "Return the decimal representation of NUM as string.

When MIN-LENGTH is specified, the result is padded on the left
with PADDING, which can be either \\='zero or \\='space (default).

Padding also happens when PADDING-NUM is specified, in that case
 MIN-LENGTH equals to the length of decimal representation of
 PADDING-NUM as string.

This function might be considered an overkill, but it\\='s used in
 short-lived scripts so often that I am tired of writing this
 kind of formatting every time."
  (if (or min-length padding-num)
      (let ((l (number-to-string
                (or min-length
                    (length (number-to-string padding-num))))))
        (format (if (eq padding 'zero)
                    (concat "%." l "d")
                  (concat "%" l ".d"))
                num))
    (number-to-string num)))

(cl-defun string-table (&key
                        data
                        header
                        header-sep
                        header-sep-start
                        header-sep-conj
                        header-sep-end
                        pad-type
                        pad-str
                        sep
                        row-start
                        row-end
                        width)
  "Format DATA as a table.

HEADER is optional. When present HEADER-SEP, HEADER-SEP-START,
HEADER-SEP-CONJ, HEADER-SEP-END control line between header and
data.

DATA is list of lists. Each column is aligned by padding with
PAD-STR either on left or right depending on value of PAD-TYPE.

The width of columns is controlled by WIDTH. If it\\='s nil, each
column takes full width. If it\\='s a list, each element must be
either \\='full or integer enabling truncation.

Each row begins with ROW-START and ends with ROW-END. Each value
in row is separated by SEP."
  (let* ((all (if header (cons header data) data))
         (n (seq-reduce
             (lambda (r v)
               (min r (if (eq 'sep v) r (seq-length v))))
             all
             (seq-length (car all))))
         (widths (seq-reduce
                  (lambda (r v)
                    (if (eq 'sep v) r
                      (seq-map-indexed
                       (lambda (a i)
                         (max
                          (pcase (or (and width
                                          (listp width)
                                          (nth i width))
                                     'full)
                            (`full (length (string-from a)))
                            (n n))
                          (or (nth i r)
                              0)))
                       v)))
                  all
                  nil))
         (pad-str (or pad-str " "))
         (pad-str-props (text-properties-at 0 pad-str))
         (pad-fns (seq-map
                   (lambda (i)
                     (pcase (or (and pad-type
                                     (listp pad-type)
                                     (nth i pad-type))
                                pad-type
                                'left)
                       (`left (lambda (len padding s)
                                (let ((extra (max 0 (- len (length s)))))
                                  (concat
                                   (apply #'propertize
                                          (make-string extra (string-to-char padding))
                                          pad-str-props)
                                   s))))
                       (`right (lambda (len padding s)
                                 (let ((extra (max 0 (- len (length s)))))
                                   (concat
                                    s
                                    (apply #'propertize
                                           (make-string extra (string-to-char padding))
                                           pad-str-props)))))))
                   (-iota n)))
         (row-start (or row-start ""))
         (row-end (or row-end ""))
         (sep (or sep " ")))
    (concat
     ;; header
     (when header
       (string-table--format-line header
         :sep sep
         :pad-fns pad-fns
         :pad-str pad-str
         :widths widths
         :row-start row-start
         :row-end row-end))
     (when header "\n")
     (when (and header header-sep)
       (string-table--format-line (-repeat n "")
         :sep (or header-sep-conj sep)
         :pad-fns pad-fns
         :pad-str header-sep
         :widths widths
         :row-start (or header-sep-start row-start)
         :row-end (or header-sep-end row-end)))
     (when (and header header-sep) "\n")
     ;; data
     (mapconcat
      (lambda (v)
        (if (and (eq 'sep v) header header-sep)
            (string-table--format-line (-repeat n "")
              :sep (or header-sep-conj sep)
              :pad-fns pad-fns
              :pad-str header-sep
              :widths widths
              :row-start (or header-sep-start row-start)
              :row-end (or header-sep-end row-end))
          (string-table--format-line (seq-take v n)
            :sep sep
            :pad-fns pad-fns
            :pad-str pad-str
            :widths widths
            :row-start row-start
            :row-end row-end)))
      data
      "\n"))))

(cl-defun string-table--format-line (values
                                     &key
                                     sep
                                     pad-fns
                                     pad-str
                                     widths
                                     row-start
                                     row-end)
  "Format lines consisting of VALUES.

Line begins with optional ROW-START and ends with optional
ROW-END.

Each value is padded with PAD-STR using PAD-FNS to achieve cell
WIDTHS. Each value is separated by SEP."
  (declare (indent 1))
  (concat
   row-start
   (string-join
    (seq-map-indexed
     (lambda (a i)
       (let* ((width-max (nth i widths))
              (str (string-from a)))
         (s-truncate
          width-max
          (funcall (nth i pad-fns)
                   width-max
                   pad-str
                   str)
          (propertize "..."
                      'help-echo str))))
     values)
    sep)
   row-end))

(provide 'lib-string)
;;; lib-string.el ends here
