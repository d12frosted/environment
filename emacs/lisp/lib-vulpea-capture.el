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
  (setq
   org-capture-templates
   '(("t" "todo" entry (file+headline vulpea-capture-inbox-file "Tasks")
      "* TODO %?\n%U\n" :clock-in t :clock-resume t)

     ("m" "Meeting" entry
      (function vulpea-capture-meeting-target)
      (function vulpea-capture-meeting-template)
      :clock-in t
      :clock-resume t)

     ("p" "Project" entry
      (function vulpea-capture-project-target)
      (function vulpea-capture-project-template)))))

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
(defun vulpea-capture-area (&optional title novisit)
  "Capture an area with TITLE.

Captured area is visited unless NOVISIT is provided."
  (interactive)
  (let ((title (or title (s-trim (read-string "Area: ")))))
    (when (string-empty-p title)
      (user-error "Area name can't be empty"))
    (when-let* ((note (vulpea-create
                      title "area/${timestamp}-${slug}.org"
                      :body (string-join
                             '("* Notes"
                               "* Research"
                               "* Action Items"
                               "* Projects"
                               "* History Log"
                               "* Archive")
                             "\n\n"))))
      (unless novisit
        (vulpea-visit note))
      note)))

;;;###autoload
(defun vulpea-capture-project ()
  "Capture a project."
  (interactive)
  (org-capture nil "p"))

(defun vulpea-capture-meeting-target ()
  "Return a target for a meeting capture."
  (let* ((person (org-capture-get :meeting-person))
         (path (if (vulpea-note-id person)
                   (vulpea-note-path person)
                 vulpea-capture-inbox-file))
         (headline "Meetings"))
    ;; unfortunately, I could not find a way to reuse
    ;; `org-capture-set-target-location'
    (set-buffer (org-capture-target-buffer path))
    ;; Org expects the target file to be in Org mode, otherwise
    ;; it throws an error. However, the default notes files
    ;; should work out of the box. In this case, we switch it to
    ;; Org mode.
    (unless (derived-mode-p 'org-mode)
      (org-display-warning
       (format
        "Capture requirement: switching buffer %S to Org mode"
        (current-buffer)))
      (org-mode))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format
                 (regexp-quote headline))
         nil t)
        (beginning-of-line)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " headline "\n")
      (beginning-of-line 0))))

(defun vulpea-capture-meeting-template ()
  "Return a template for a meeting capture."
  (let ((person (vulpea-select-from
                 "Person"
                 (vulpea-db-query-by-tags-every '("people")))))
    (org-capture-put :meeting-person person)
    (if (vulpea-note-id person)
        "* MEETING [%<%Y-%m-%d %a>] :REFILE:MEETING:\n%U\n\n%?"
      (concat "* MEETING with "
              (vulpea-note-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

(defun vulpea-capture-project-target ()
  "Return a target for a project capture."
  (let ((area (org-capture-get :project-area)))
    ;; unfortunately, I could not find a way to reuse
    ;; `org-capture-set-target-location'
    (let ((path (vulpea-note-path area))
          (headline "Tasks"))
      (set-buffer (org-capture-target-buffer path))
      ;; Org expects the target file to be in Org mode, otherwise
      ;; it throws an error. However, the default notes files
      ;; should work out of the box. In this case, we switch it to
      ;; Org mode.
      (unless (derived-mode-p 'org-mode)
        (org-display-warning
         (format
          "Capture requirement: switching buffer %S to Org mode"
          (current-buffer)))
        (org-mode))
      (org-capture-put-target-region-and-position)
      (widen)
      (goto-char (point-min))
      (if (re-search-forward
           (format org-complex-heading-regexp-format
                   (regexp-quote headline))
           nil t)
          (beginning-of-line)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " headline "\n")
        (beginning-of-line 0)))))

(defun vulpea-capture-project-template ()
  "Return a template for a project capture."
  (let* ((area (vulpea-select-from
                "Area"
                (->> (vulpea-db-query-by-tags-every '("area"))
                     (--filter (= 0 (vulpea-note-level it))))))
         (area (if (vulpea-note-id area)
                   area
                 (vulpea-capture-area (vulpea-note-title area) :no-visit)))
         (title (s-trim (read-string "Project: "))))
    (when (string-empty-p title)
      (user-error "Project name can't be empty"))
    (org-capture-put :project-area area)
    (string-join
     (list (concat "* TODO " title " :project:")
           ":PROPERTIES:"
           (format org-property-format ":CATEGORY:"
                   (concat
                    (or (vulpea-note-meta-get area "short name")
                        (vulpea-note-title area))
                    " > "
                    title))
           ":END:"
           (format-time-string (org-time-stamp-format t 'inactive))
           ""
           "%?")
     "\n")))

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
