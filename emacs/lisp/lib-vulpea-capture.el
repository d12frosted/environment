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
               t)
  ;; and of course, we need journal capture
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry
                 (function vulpea-capture--journal-target)
                 "* %<%H:%M> %?")))

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

;; The journal capture target is a function, and `org-capture' calls it
;; without arguments, so the date travels through a dynamic variable that
;; `vulpea-capture-journal' let-binds around the capture.

(declare-function vulpea-journal-capture-target "vulpea-journal" (&optional date))
(declare-function vulpea-journal-date-next "vulpea-journal" ())
(declare-function vulpea-journal-date-previous "vulpea-journal" ())

(defvar vulpea-capture--journal-date nil
  "Date to capture the journal entry for.

When nil, the entry goes to today's journal note.")

(defun vulpea-capture--journal-target ()
  "Set buffer and point for the journal capture template.

Targets `vulpea-capture--journal-date', or today when it is nil."
  (vulpea-journal-capture-target vulpea-capture--journal-date))

(defun vulpea-capture--journal-read-date ()
  "Read a date to capture a journal entry for.

Reading works just like in `vulpea-journal-date': future dates are
not preferred, and \\`M-<left>' and \\`M-<right>' jump between
dates that already have a journal entry."
  (require 'vulpea-journal)
  (minibuffer-with-setup-hook
      (lambda ()
        (local-set-key (kbd "M-<left>") #'vulpea-journal-date-previous)
        (local-set-key (kbd "M-<right>") #'vulpea-journal-date-next))
    (let ((org-read-date-prefer-future nil))
      (org-time-string-to-time
       (org-read-date nil nil nil "Journal date: ")))))

;;;###autoload
(defun vulpea-capture-journal ()
  "Capture a journal entry.

By default it uses current date to find a daily. With
\\[universal-argument] user may select the date."
  (interactive)
  (let ((vulpea-capture--journal-date
         (when (equal current-prefix-arg '(4))
           (vulpea-capture--journal-read-date))))
    (org-capture nil "j")))

(provide 'lib-vulpea-capture)
;;; lib-vulpea-capture.el ends here
