;;; lib-hash-table.el --- Hash table utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 06 Apr 2023
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
;;; Code:

(require 'dash)

(cl-defun hash-table-from (list &key key-fn value-fn test)
  "Convert LIST to hash table.

KEY-FN (required) is a function to convert a given value from LIST to key.

VALUE-FN (optional) is a function to convert a given value from
list to value. It accepts two arguments - element of list and
current value assigned to key in hast table. The latter allows to
deal with overlapping keys. VALUE-FN by default returns a given
element of list as is.

TEST must be a symbol that specifies how to compare keys. Default
is ‘equal’. Predefined are the tests ‘eq’, ‘eql’, and ‘equal’.
User-supplied test and hash functions can be specified via
‘define-hash-table-test’."
  (declare (indent 1))
  (let ((tbl (make-hash-table :test (or test 'equal)))
        key val)
    (--each list
      (setq key (funcall key-fn it))
      (setq val (if value-fn
                    (funcall value-fn it (gethash key tbl))
                  it))
      (puthash key val tbl))
    tbl))

(provide 'lib-hash-table)
;;; lib-hash-table.el ends here
