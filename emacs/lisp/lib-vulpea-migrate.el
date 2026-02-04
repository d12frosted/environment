;;; lib-vulpea-migrate.el --- Migration utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Feb 2026
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
;; Migration utilities for vulpea notes.  Includes functions to backfill
;; :ID: and :CREATED: properties on existing TODO items.
;;
;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)

;;;###autoload
(defun vulpea-migrate-todos ()
  "Add :ID: and :CREATED: to all TODOs in agenda files.

- Adds :ID: if missing (required for vulpea indexing)
- Adds :CREATED: from LOGBOOK if missing (for date queries)

Run `vulpea-db-sync' after migration to index the updated notes."
  (interactive)
  (let ((count-id 0) (count-created 0))
    (org-map-entries
     (lambda ()
       ;; Add ID if missing
       (unless (org-entry-get nil "ID")
         (org-id-get-create)
         (cl-incf count-id))
       ;; Add CREATED if missing
       (unless (org-entry-get nil "CREATED")
         (when-let* ((created (vulpea-migrate--find-earliest-logbook-date)))
           (org-set-property "CREATED" created)
           (cl-incf count-created))))
     "TODO={.+}" 'agenda)
    (message "Migration complete: %d IDs added, %d CREATED added" count-id count-created)))

(defun vulpea-migrate--find-earliest-logbook-date ()
  "Find earliest timestamp in current entry's LOGBOOK drawer.

Parses state change entries like:
  - State \"DONE\" from \"TODO\" [2024-08-26 Mon 11:16]

Returns the earliest date found in format \"[YYYY-MM-DD]\", or nil
if no LOGBOOK drawer or timestamps found."
  (save-excursion
    (let ((bound (save-excursion (org-end-of-subtree t)))
          (earliest nil))
      (when (re-search-forward ":LOGBOOK:" bound t)
        (let ((drawer-end (save-excursion
                            (re-search-forward ":END:" bound t))))
          (while (re-search-forward
                  "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^]]*\\]"
                  drawer-end t)
            (let ((date (match-string 1)))
              (when (or (null earliest) (string< date earliest))
                (setq earliest date))))))
      (when earliest
        (format "[%s]" earliest)))))

(provide 'lib-vulpea-migrate)
;;; lib-vulpea-migrate.el ends here
