;;; emacs/buffer/autoload/messages.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Dec 2018
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

(defvar +buffer-messages-display-fn #'+buffer-display-and-switch
  "The function to use to display messages buffer.

Must accept one argument: the buffer to display.")

;;;###autoload
(defvar +buffer-messages-name "*Messages*"
  "The name of the messages buffer.")

;;;###autoload
(defun +buffer/pop-messages ()
  "Open `+buffer-messages-name' buffer."
  (interactive)
  (funcall +buffer-messages-display-fn
           (get-buffer-create +buffer-messages-name)))

;;;###autoload
(defun +buffer/switch-to-messages ()
  "Open `+buffer-messages-name' buffer."
  (interactive)
  (let ((+buffer-messages-display-fn #'switch-to-buffer))
    (+buffer/open-messages)))
