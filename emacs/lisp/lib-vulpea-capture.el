;;; lib-vulpea-capture.el --- Capturing tasks and notes -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 11 Feb 2021
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
;; Org-capture templates and target functions for tasks, meetings, areas,
;; and projects. Meetings are captured under person notes, projects under
;; area notes.
;;
;;; Code:

(require 'init-env)
(require 'config-vulpea)
(require 'lib-fun)

;; external dependencies
(require 'vulpea)
(require 'org)
(require 'org-capture)
(require 'org-cliplink)

(defvar vulpea-capture-inbox-file "inbox.org"
  "The path to the inbox file.

It is relative to `vulpea-directory', unless it is absolute.")

;;;###autoload
(defun vulpea-capture-setup ()
  "Wire all bits for capturing."
  (dolist (var '(vulpea-capture-inbox-file))
    (set var (expand-file-name (symbol-value var) vulpea-directory)))
  (unless org-default-notes-file
    (setq org-default-notes-file vulpea-capture-inbox-file))
  ;; the project ("p") and meeting ("m") templates come from
  ;; `vulpea-para-setup-defaults'; only the personal inbox todo is added
  ;; here, appended so it composes with them
  (add-to-list 'org-capture-templates
               '("t" "todo" entry
                 (file+headline vulpea-capture-inbox-file "Tasks")
                 (function vulpea-para-capture-task-template)
                 :clock-in t :clock-resume t)
               t))

;; Capture templates and targets, and area creation, live in vulpea-para:
;; the task template (`vulpea-para-capture-task-template'), and the
;; project and meeting targets/templates installed as the "p"/"m" keys by
;; `vulpea-para-setup-defaults', plus `vulpea-para-capture-area'.  The
;; commands below are thin wrappers over those capture keys.

;;;###autoload
(defun vulpea-capture-task ()
  "Capture a task."
  (interactive)
  (org-capture nil "t"))

;;;###autoload
(defun vulpea-capture-meeting ()
  "Capture a meeting."
  (interactive)
  (org-capture nil "m"))

;;;###autoload
(defun vulpea-capture-project ()
  "Capture a project."
  (interactive)
  (org-capture nil "p"))

;; Meeting capture (target + template) now lives in vulpea-para as
;; `vulpea-para-capture-meeting-target' and
;; `vulpea-para-capture-meeting-template'.

;;;###autoload
(defun vulpea-capture-journal ()
  "Capture a journal entry.

By default it uses current date to find a daily. With
\\[universal-argument] user may select the date."
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))     ; select date
    (user-error "Not implemented"))
   (t
    (user-error "Not implemented"))))

(provide 'lib-vulpea-capture)
;;; lib-vulpea-capture.el ends here
