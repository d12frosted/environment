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
;; Change collection is implemented on top of
;; `vulpea-buffer-meta-change-functions', the abnormal hook that
;; vulpea runs whenever buffer metadata changes. This keeps the logger
;; decoupled from vulpea's internal call signatures.
;;
;;; Code:

(require 'dash)
(require 'vulpea)

(defvar vcl--data nil
  "A table of all changes.

Key is note id and the value is list of (meta old new).")

(defun vcl-start ()
  "Start Vulpea change logger."
  (setq vcl--data (make-hash-table :test 'equal))
  (add-hook 'vulpea-buffer-meta-change-functions #'vcl--record))

(defun vcl-stop ()
  "Stop Vulpea change logger."
  (remove-hook 'vulpea-buffer-meta-change-functions #'vcl--record))

(defun vcl--record (prop old new)
  "Record a metadata change of PROP from OLD to NEW.

OLD and NEW are lists of string values, as documented in
`vulpea-buffer-meta-change-functions'.  The change is filed under
the id of the note in the current buffer."
  (when-let* ((id (save-excursion
                    (goto-char (point-min))
                    (org-id-get))))
    (puthash id
             (-snoc (gethash id vcl--data) (list prop old new))
             vcl--data)))

(defun vcl-report ()
  "Print all data from logger."
  (--each (vulpea-db-query-by-ids (hash-table-keys vcl--data))
    (message "* %s" (vulpea-note-title it))
    (--each (gethash (vulpea-note-id it) vcl--data)
      (message "  property: %s\n    before: %S\n     after: %S" (nth 0 it) (nth 1 it) (nth 2 it)))
    (message "")))

(provide 'lib-vcl)
;;; lib-vcl.el ends here
