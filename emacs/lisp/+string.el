;;; +string.el --- string utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Nov 2019
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

;;;###autoload
(defun +string-match-1 (regexp val)
  "Get the first group from REGEXP match of the VAL.

VAL can be either a string or a region (beg . end) of the
buffer."
  (+string-match-n 1 regexp val))

;;;###autoload
(defun +string-match-n (n regexp val)
  "Get the Nth group from REGEXP match of the VAL.

VAL can be either a string or a region (beg . end) of the
buffer."
  (let ((s (if (stringp val)
               val
             (buffer-substring (car val) (cdr val)))))
    (string-match regexp s)
    (match-string n s)))

(defun +string-join (strs sep)
  "Join a list of STRS using SEP."
  (pcase strs
    (`nil "")
    (`(,str) str)
    (_ (mapconcat #'identity strs sep))))

(defun +string-chop-prefix-regexp (prefix s)
  "Remove PREFIX regexp if it is at the start of S."
  (message "(+string-chop-prefix-regexp %s %s)" prefix s)
  (s-chop-prefix (car (s-match prefix s)) s))

(defun +string-chop-suffix-regexp (suffix s)
  "Remove SUFFIX regexp if it is at the end of S."
  (s-chop-suffix (car (s-match suffix s)) s))

(provide '+string)
;;; +string.el ends here
