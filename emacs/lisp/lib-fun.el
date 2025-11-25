;;; lib-fun.el --- Extra functions and utlities -*- lexical-binding: t; -*-
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
;; Function utilities: unquoting, interactive loops with collection or
;; filtering, suppressing messages, and removing keyword parameters.
;;
;;; Code:

(defun fun-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun fun-collect-while (fn filter &rest args)
  "Repeat FN and collect it's results until `C-g` is used.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

If FILTER is nil, it does not affect repeat cycle.

If FILTER returns nil, the computed value is not added to result.

ARGS are passed to FN."
  (let (result
        value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (if (and filter
                 (null (funcall filter value)))
            (setq continue nil)
          (setq result (cons value result)))))
    (setq quit-flag nil)
    (seq-reverse result)))

(defun fun-repeat-while (fn filter &rest args)
  "Repeat FN and return the first unfiltered result.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

ARGS are passed to FN."
  (let (value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (when (null (funcall filter value))
          (setq continue nil))))
    (setq quit-flag nil)
    (when (null continue)
      value)))

(defun fun-noop (&rest _)
  "Do nothing.

Useful for temporarily disabling a function.")

(defun fun-remove-keyword-params (seq)
  "Remove keyword parameters from arguments SEQ.

Useful for cases, when you want to combine &rest and &key
arguments in `cl-defun'. For example:

  (cl-defun foo (&rest rest
                 &key key1 key2
                 &allow-other-keys)
    (fun-remove-keyword-params rest))"
  (if (null seq) nil
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (keywordp head) (fun-remove-keyword-params (cdr tail))
        (cons head (fun-remove-keyword-params tail))))))

(defun fun-silent (fn &rest args)
  "Wrapper for FN suppressing any messages.

ARGS are passed as is to FN."
  (cl-letf (((symbol-function 'message) #'format))
    (apply fn args)))

(defmacro silenzio (&rest body)
  "Evaluate BODY like in Vatican. Silently."
  `(cl-letf (((symbol-function 'message) #'format))
    ,@body))

(provide 'lib-fun)
;;; lib-fun.el ends here
