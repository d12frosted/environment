;;; lib-vino.el --- Vino utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
;; Various helpers and utilities extending `vino' module. They are
;; here because there is no place for them in the upstream package.
;;
;;; Code:

(require 'init-elpa)
(require 'lib-inventory)
(require 'vino)

(defvar vino-inventory-file nil
  "Path to journal file.")

;;;###autoload
(defun vino-availability-get (id)
  "Get availability info for `vino-entry' with ID."
  (cons
   (inventory-total-in vino-inventory-file id)
   (inventory-total-out vino-inventory-file id)))

;;;###autoload
(defun vino-availability-add (id amount source date)
  "Add AMOUNT of `vino-entry' with ID from SOURCE on DATE."
  (inventory-add vino-inventory-file id amount source date))

;;;###autoload
(defun vino-availability-sub (id amount action date)
  "Subtrack AMOUNT of `vino-entry' with ID by ACTION on DATE."
  (inventory-sub vino-inventory-file id amount action date))

;;;###autoload
(defun vino-entry-find-file-available ()
  "Select and visit available `vino-entry'."
  (interactive)
  (let* ((available (seq-map
                     #'car
                     (inventory-balance-list
                      vino-inventory-file)))
         (res (vulpea-select
               "Wine"
               :filter-fn
               (lambda (note)
                 (let ((tags (vulpea-note-tags note)))
                   (and (seq-contains-p tags "wine")
                        (seq-contains-p tags "cellar")
                        (seq-contains-p available
                                        (vulpea-note-id note))))))))
    (if (vulpea-note-id res)
        (find-file (vulpea-note-path res))
      (user-error
       "Can not visit vino entry that does not exist: %s"
       (vulpea-note-title res)))))

;;;###autoload
(defun vino-balance-display ()
  "Display buffer with balances."
  (interactive)
  (let* ((name "*vino inventory*")
         (buf (get-buffer name)))
    (when buf
      (kill-buffer buf))
    (setq buf (generate-new-buffer name))
    (with-current-buffer buf
      (let ((balances (inventory-balance-list vino-inventory-file)))
        (seq-do
         (lambda (kvp)
           (let* ((id (car kvp))
                  (am (cdr kvp))
                  (note (vulpea-db-get-by-id id)))
             (insert (format "%05.2f" am)
                     "  "
                     (vulpea-note-title note)
                     "\n")))
         balances)
        (insert "\n"
                "---"
                "\n"
                "total: "
                (format "%05.2f"
                        (seq-reduce
                         (lambda (r v) (+ r (cdr v)))
                         balances
                         0))
                " ("
                (format "%i"
                        (seq-reduce
                         (lambda (r v) (+ r (max 1 (cdr v))))
                         balances
                         0))
                " bottles)"))
      (read-only-mode))
    (display-buffer buf)))

(provide 'lib-vino)
;;; lib-vino.el ends here
