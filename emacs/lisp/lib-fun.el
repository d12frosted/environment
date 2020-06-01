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

(defun +repeat-fn (fn &rest args)
  "Repeat FN and collect it's results until `C-g` is used.

ARGS are passed to FN."
  (let (result (inhibit-quit t))
    (with-local-quit
      (while t
        (setq result (cons (apply fn args) result))))
    (setq quit-flag nil)
    (seq-reverse result)))

(provide 'lib-fun)
;;; lib-fun.el ends here
