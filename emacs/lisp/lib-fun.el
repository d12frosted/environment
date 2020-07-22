;;; lib-fun.el --- functions and stuff -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Jun 2020
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

(defun +unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun +repeat-fn (fn &rest args)
  "Repeat FN and collect it's results until `C-g` is used.

ARGS are passed to FN."
  (let (result (inhibit-quit t))
    (with-local-quit
      (while t
        (setq result (cons (apply fn args) result))))
    (setq quit-flag nil)
    (seq-reverse result)))

(defmacro eval-with-default-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

(provide 'lib-fun)
;;; lib-fun.el ends here
