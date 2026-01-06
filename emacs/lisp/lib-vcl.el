;;; lib-vcl.el --- Vulpea Change Logger -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 05 Feb 2025
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
;; This module implements utilities for collecting and reporting
;; changes to vulpea notes. Currently it only supports metadata
;; changes.
;;
;; Useful when doing batch updates or during maintenance procedures.
;;
;; Here is a typical usage pattern:
;;
;;   1. start collecting changes
;;   (vcl-start)
;;
;;   2. process a batch of notes
;;   (vulpea-utils-process-notes (vulpea-db-query-by-tags-every '("wine" "cellar"))
;;     (vulpea-buffer-meta-set "prop" "value" 'append))
;;
;;   3. stop collecting changes
;;   (vcl-stop)
;;
;;   4. report to the message buffer all changes
;;   (vcl-report)
;;
;;; Code:

(require 'dash)
(require 'vulpea)

(defvar vcl--data nil
  "A table of all changes.

Key is note id and the value is list of (meta old new).")

(defvar vcl--supress nil)

(defun vcl-start ()
  "Start Vulpea change logger."
  (setq vcl--data (make-hash-table :test 'equal))
  (advice-add 'vulpea-buffer-meta-set :around #'vcl--buffer-meta-set)
  (advice-add 'vulpea-buffer-meta-remove :around #'vcl--buffer-meta-remove)
  (advice-add 'vulpea-buffer-meta-clean :around #'vcl--buffer-meta-clean)
  (advice-add 'vulpea-buffer-meta-sort :around #'vcl--buffer-meta-sort))

(defun vcl-stop ()
  "Stop Vulpea change logger."
  (advice-remove 'vulpea-buffer-meta-set #'vcl--buffer-meta-set)
  (advice-remove 'vulpea-buffer-meta-remove #'vcl--buffer-meta-remove)
  (advice-remove 'vulpea-buffer-meta-clean #'vcl--buffer-meta-clean)
  (advice-remove 'vulpea-buffer-meta-sort #'vcl--buffer-meta-sort))

(defun vcl-report ()
  "Print all data from logger."
  (--each (vulpea-db-query-by-ids (hash-table-keys vcl--data))
    (message "* %s" (vulpea-note-title it))
    (--each (gethash (vulpea-note-id it) vcl--data)
      (message "  property: %s\n    before: %S\n     after: %S" (nth 0 it) (nth 1 it) (nth 2 it)))
    (message "")))

(defun vcl--log (id prop old new)
  "Log change of PROP in note with ID.

OLD is the previous value and NEW is the new value."
  (when (and id (not vcl--supress))
    (let ((changed (cond
                    ((and (listp old)
                          (not (listp new))
                          (= 1 (length old)))
                     (not (string-equal (nth 0 old) (vulpea-buffer-meta-format new))))
                    ((and (listp old)
                          (listp new)
                          (= (length old)
                             (length new)))
                     (--some (not (string-equal (car it)
                                                (vulpea-buffer-meta-format (cdr it))))
                             (-zip-pair old new)))
                    (t t)))
          (changes (gethash id vcl--data))
          (oldf (if (and (listp old)
                          (not (listp new))
                          (= 1 (length old)))
                    (nth 0 old)
                  old))
          (newf (if (and new (not (listp new)))
                    (vulpea-buffer-meta-format new)
                  new)))
      (when changed
        (puthash id (-snoc changes (list prop oldf newf)) vcl--data)))))

(defun vcl--buffer-meta-set (fn prop value &optional append)
  "A VCL wrapper for `vulpea-buffer-meta-set' FN.

Refer to original function to learn about PROP, VALUE and APPEND
meaning."
  (let ((id (save-excursion
              (goto-char (point-min))
              (org-id-get)))
        (old (vulpea-buffer-meta-get-list prop 'string)))
    (vcl--log id prop old value)
    (let ((vcl--supress t))
      (funcall fn prop value append))))

(defun vcl--buffer-meta-remove (fn prop)
  "A VCL wrapper for `vulpea-buffer-meta-remove' FN.

Refer to original function to learn about PROP meaning."
  (let ((id (save-excursion
              (goto-char (point-min))
              (org-id-get)))
        (old (vulpea-buffer-meta-get-list prop 'string)))
    (vcl--log id prop old nil)
    (funcall fn prop)))

(defun vcl--buffer-meta-clean (fn)
  "A VCL wrapper for `vulpea-buffer-meta-clean' FN."
  (let ((id (save-excursion
              (goto-char (point-min))
              (org-id-get)))
        (meta (vulpea-buffer-meta)))
    (--each (vulpea-buffer-meta-props meta)
      (vcl--log id it (vulpea-buffer-meta-get! meta it) nil))
    (funcall fn)))

(defun vcl--buffer-meta-sort (fn props)
  "A VCL wrapper for `vulpea-buffer-meta-sort' FN.

Refer to original function to learn about PROPS meaning."
  (let ((vcl--supress t))
    (funcall fn props)))

(provide 'lib-vcl)
;;; lib-vcl.el ends here
