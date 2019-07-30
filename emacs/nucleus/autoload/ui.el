;;; ui.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;; Most of the code was borrowed from hlissner/doom-emacs.
;;
;;; Commentary:
;;
;;; Code:

;;
;; Advice

;;;###autoload
(defun nucleus*shut-up (orig-fn &rest args)
  "Generic advisor for silencing noisy functions."
  (quiet! (apply orig-fn args)))
