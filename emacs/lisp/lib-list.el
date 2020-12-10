;;; lib-list.el --- list utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Nov 2019
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

(defun +seq-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun +seq-flatten (list-of-lists)
  "Flatten LIST-OF-LISTS."
  (apply #'append list-of-lists))

(defun +seq-singleton (list)
  "Return the only element of the LIST.

Return nil, if the LIST contains more than one element."
  (cond ((listp list) (pcase list
                        (`(,l . nil) l)))
	      (t list)))

(provide 'lib-list)
;;; lib-list.el ends here
