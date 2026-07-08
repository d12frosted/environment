;;; lib-vulpea-schemas.el --- Note schemas -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 08 Jul 2026
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
;; Declarative `vulpea-schema' definitions for my notes.  These are the
;; single source of truth for the metadata format consumers expect;
;; publishing pipelines (e.g. tasogare.ink's build-rules.el) validate
;; notes against them on pull, so malformed metadata fails the export
;; instead of breaking a downstream deploy.
;;
;;; Code:

(require 'vulpea-schema)

;;; tasogare.ink

(defconst tasogare-work-statuses
  '("completed" "in progress" "reading" "ongoing" "dropped" "planned")
  "Allowed values for the \"status\" field of tasogare work notes.
Must stay in sync with WORK_STATUSES in tasogare's lib/types.ts.")

(vulpea-schema-define 'tasogare-work
  :predicate (lambda (note)
               (vulpea-note-tagged-all-p note "tasogare" "works"))
  :fields
  `((:key "status" :one-of ,tasogare-work-statuses)
    (:key "publish" :one-of ("true" "false"))
    (:key "rating" :type number)))

(vulpea-schema-define 'tasogare-post
  :predicate (lambda (note)
               (vulpea-note-tagged-all-p note "tasogare" "posts"))
  :fields
  '((:key "publish" :one-of ("true" "false"))
    (:key "works" :type note :multiple t :target-tags ("tasogare" "works"))
    (:key "related" :type note :multiple t :target-tags ("tasogare"))))

(provide 'lib-vulpea-schemas)
;;; lib-vulpea-schemas.el ends here
