;;; lib-list.el --- list helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 30 Nov 2023
;;
;; URL: https://github.com/d12frosted/
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
;; List utilities extending dash.el: 1-based position finding, element
;; counting, and conditional updates.
;;
;;; Code:

(require 'dash)

;; * Position (1-based index)

(defun -find-position (list pred)
  "Find first position in LIST satisfying PRED.

Position is 1-based, while index is 0-based."
  (when-let ((idx (-find-index pred list)))
    (1+ idx)))

(defun -position-of (list str)
  "Find first position of STR in LIST .

Position is 1-based, while index is 0-based."
  (-find-position list (-partial #'string-equal str)))

(defun -find-positions (list pred)
  "Find all positions in LIST satisfying PRED.

Position is 1-based, while index is 0-based."
  (when-let ((idxs (-find-indices pred list)))
    (-map #'1+ idxs)))

(defun -positions-of (list str)
  "Find all positions of STR in LIST.

STR can be a list of strings.

Position is 1-based, while index is 0-based."
  (if (stringp str)
      (-find-positions list (-partial #'string-equal str))
    (-find-positions list (-partial #'-contains-p str))))

;; * misc

(defun -update-first-by (pred fn def list)
  "Update elements of LIST using FN.

Return a copy of LIST, where the first element matching PRED is
updated using FN. In case no element is found, DEF is added to the end."
  (if-let* ((n (-find-index pred list)))
      (-update-at n fn list)
    (-snoc list def)))

(defmacro --update-first-by (pred fn def list)
  "Anaphoric version of `-update-by'.

PRED is a form with `it' bound to element of the LIST.
FN is a form with `it' bound to element of the LIST.
DEF is used when no element is found."
  (declare (debug (form def-form form)))
  `(-update-first-by (lambda (it) (ignore it) ,pred) (lambda (it) (ignore it) ,fn) ,def ,list))

(defun -count-unique (list)
  "Return count unique elements of LIST."
  (cl-loop
   with hash = (make-hash-table :test #'equal)
   for key in list
   do (cl-incf (gethash key hash 0))
   finally (return
            (cl-loop for key being each hash-key of hash
                     using (hash-value value)
                     collect (cons key value)))))

(provide 'lib-list)
;;; lib-list.el ends here
