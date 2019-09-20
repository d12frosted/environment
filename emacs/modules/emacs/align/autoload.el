;;; emacs/align/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Sep 2019
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defalias #'+align-regexp #'align-regexp)

;;;###autoload
(defun +align/= (beg end)
  (interactive "r")
  (+align-regexp beg end "\\(\\s-*\\)="))

;;;###autoload
(defun +align/:: (beg end)
  (interactive "r")
  (+align-regexp beg end "\\(\\s-*\\)::"))

;;;###autoload
(defun +align/lang-ext (beg end)
  (interactive "r")
  (+align-regexp beg end "\\(\\s-*\\)#-}"))
