;;; +inventory.el --- inventory management -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Nov 2019
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

(require 'subr-x)

;;;###autoload
(defun inventory-balance (file id &optional query)
  "Get balance of ID in FILE using QUERY."
  (let* ((cmd (format "hledger -f %s b %s '%s'" file id query))
         (res (shell-command-to-string cmd))
         (lines (split-string res "\n")))
    (string-to-number (car (seq-drop-while #'string-empty-p (reverse lines))))))

;;;###autoload
(defun inventory-total-in (file id)
  "Get total income for ID in FILE."
  (inventory-balance file id "amt:>0"))

;;;###autoload
(defun inventory-total-out (file id)
  "Get total outcome for ID in FILE."
  (abs (inventory-balance file id "amt:<0")))

;;;###autoload
(defun inventory-add (file id amount source &optional date)
  "Add AMOUNT of ID to inventory in FILE from SOURCE.

When DATE is omitted, `current-time' is used."
  (shell-command-to-string
   (format
    "echo '\n%s\n    goods:%s  %s\n    source:%s' >> '%s'"
    (format-time-string "%Y/%m/%d" date)
    id
    amount
    source
    file)))

;;;###autoload
(defun inventory-sub (file id amount action &optional date)
  "Subtract amount of ID from inventory in FILE as result of ACTION.

When DATE is omitted, `current-time' is used."
  (shell-command-to-string
   (format
    "echo '\n%s\n    activity:%s  %s\n    goods:%s' >> %s"
    (format-time-string "%Y/%m/%d" date)
    action
    amount
    id
    file)))

(provide '+inventory)
;;; +inventory.el ends here
