;;; lib-plist.el --- Property list utilitites -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Feb 2021
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
;; Utilities for property lists.
;;
;;; Code:

(require 'lib-string)

(defun plist-delete (plist prop)
  "Delete PROP from PLIST."
  (let (p)
    (while plist
      (if (not (eq prop (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defmacro plist-buttonize-prop (plist prop default callback
                                      &optional to-string read-fn)
  "Make a button out of PROP from PLIST.

The result is a button string. The extracted value (or DEFAULT)
is displayed using TO-STRING or `string-from'. The action prompts
a new value unless READ-FN is specified.

CALLBACK is called with updated plist."
  (let* ((plist-var (gensym))
         (prompt (concat
                  (s-capitalize
                   (s-replace "-" " " (s-chop-prefix ":" (symbol-name prop))))
                  ": "))
         (to-string (or to-string `(quote string-from))))
    `(let* ((,plist-var ,(if (and (listp plist) (not (eq (car plist) 'quote)))
                             `(quote ,plist)
                           plist))
            (value (or (plist-get ,plist-var ,prop) ,default))
            (read-fn (or
                      ,read-fn
                      (pcase (type-of value)
                       (`integer #'read-number)
                       (_ #'read-string)))))
      (buttonize (funcall ,to-string value)
       (lambda (&rest _)
         (let* ((value (funcall read-fn ,prompt))
                (,plist-var (plist-put ,plist-var ,prop value)))
          (funcall ,callback ,plist-var)))))))

(provide 'lib-plist)
;;; lib-plist.el ends here
