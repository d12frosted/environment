;;; lib-inventory.el --- Inventory management -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 13 Feb 2021
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
;; Utilities for managiing inventory using hledger.
;;
;;; Code:

;;;###autoload
(defun inventory-balance (file id &optional query)
  "Get balance of ID in FILE using QUERY."
  (let* ((cmd (format "hledger -f %s balance %s '%s'" file id query))
         (res (shell-command-to-string cmd))
         (lines (split-string res "\n")))
    (string-to-number
     (car (seq-drop-while #'string-empty-p (reverse lines))))))

;;;###autoload
(defun inventory-balance-list (file)
  "Get balance of all entries in FILE."
  (let* ((cmd (concat "hledger -f '"
                      file
                      "' balance 'goods:' "
                      "--format '%(account) %(total)'"))
         (res (shell-command-to-string cmd))
         (lines (split-string res "\n")))
    (seq-reduce
     (lambda (r x)
       (if (string-prefix-p "goods:" x)
           (let* ((pair (split-string x " "))
                  (id (string-remove-prefix "goods:" (car pair)))
                  (amt (string-to-number (nth 1 pair))))
             (cons (cons id amt) r))
         r))
     lines
     nil)))

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

;;;###autoload
(defun inventory-sources (file)
  "Get the list of sources in FILE."
  (let* ((cmd (concat "hledger -f '" file "' accounts"))
         (res (shell-command-to-string cmd))
         (lines (split-string res "\n")))
    (seq-map
     (lambda (s) (string-remove-prefix "source:" s))
     (seq-filter
      (lambda (s) (string-prefix-p "source:" s))
      lines))))

(provide 'lib-inventory)
;;; lib-inventory.el ends here
