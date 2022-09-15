;;; lib-table.el --- Table utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 15 Sep 2022
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
;; Table is a list of lists. These utilities might be slow.
;;
;;; Code:


;; * Selection

(cl-defun table-select-rows (value table &key column compare-fn)
  "Select rows with a VALUE at COLUMN from TABLE.

COLUMN is zero-based.

COMPARE-FN is a function used to compare values. By default
`string-equal' is used.

Everything before and including COLUMN is dropped from the rows."
  (setf column (or column 0))
  (-map
   (-partial #'-drop (+ 1 column))
   (-filter
    (-compose
     (-partial (or compare-fn #'string-equal) value)
     (-partial #'nth column))
    table)))


;; * Manipulations

(cl-defun table-transpose (table)
  "Transpose TABLE."
  (let ((n (length (car table))))
    (-map
     (lambda (i)
       (-map (-partial #'nth i) table))
     (-iota n))))


;; * Application

(cl-defun table-vreduce-columns (fn table)
  "Reduce columns of the TABLE using FN.

FN is a function that takes a list of `calc' numbers and returns
a `calc' number. Result is converted into Emacs Lisp number.

Empty cells of the column are ignored (e.g. filtered out).

In short, this function allows to apply `calc' vector function on
columns, but also supports missing data."
  (-map (-compose #'calc-to-number
                  (-applify fn)
                  (-partial #'-map #'calc-from-number))
        (-filter
         #'identity
         (-map (-partial #'-filter #'numberp) (table-transpose table)))))



(provide 'lib-table)
;;; lib-table.el ends here
