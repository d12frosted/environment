;;; lib-compat.el --- Compatibility utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Feb 2021
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
;; While these configurations are aimed for bleeding edge of Emacs,
;; compatibility with latest released version of Emacs is possible.
;; This module defines missing bits.
;;
;;; Code:

;;;###autoload
(defun cl--plist-remove (plist member)
  "Remove MEMBER from PLIST.

`telega' needs this function."
  (cond
   ((null plist) nil)
   ((null member) plist)
   ((eq plist member) (cddr plist))
   (t `(,(car plist) ,(cadr plist)
        ,@(cl--plist-remove (cddr plist) member)))))

(provide 'lib-compat)
;;; lib-compat.el ends here
