;;; lib-vulpea-agenda.el --- Utilities for building agenda -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 12 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
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
;; This module provides various utilities for building agenda buffer.
;;
;;; Code:

(require 'init-elpa)
(require 'lib-vulpea)
(require 'lib-compat)

(require 'org)
(require 'org-habit)
(require 'vulpea)



(defvar vulpea-agenda-hide-scheduled-and-waiting-next-tasks t
  "Non-nil means to hide scheduled and waiting tasks.

Affects the following commands:

- `vulpea-agenda-cmd-focus'
- `vulpea-agenda-cmd-waiting'")

(defvar vulpea-agenda-main-buffer-name "*agenda:main*"
  "Name of the main agenda buffer.")



;;;###autoload
(defun vulpea-agenda-main ()
  "Show main `org-agenda' view."
  (interactive)
  (org-agenda nil " "))

;;;###autoload
(defun vulpea-agenda-person ()
  "Show main `org-agenda' view."
  (interactive)
  (let* ((person (vulpea-select
                  "Person"
                  :filter-fn
                  (lambda (note)
                    (seq-contains-p (vulpea-note-tags note)
                                    "people"))))
         (names (seq-map
                 #'car
                 (org-roam-db-query
                  [:select title
                   :from titles
                   :where (= file $s1)]
                  (vulpea-note-path person))))
         (tags (seq-map #'vulpea--title-to-tag names))
         (query (string-join tags "|")))
    (dlet ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))



;;;###autoload
(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))


;; Commands

;;;###autoload
(defconst vulpea-agenda-cmd-refile
  '(tags
    "REFILE"
    ((org-agenda-overriding-header "To refile")
     (org-tags-match-list-sublevels nil))))

;;;###autoload
(defconst vulpea-agenda-cmd-today
  '(agenda
    ""
    ((org-agenda-span 'day)
     (org-agenda-skip-deadline-prewarning-if-scheduled t)
     (org-agenda-sorting-strategy '(habit-down
                                    time-up
                                    category-keep
                                    todo-state-down
                                    priority-down)))))

;;;###autoload
(defconst vulpea-agenda-cmd-focus
  '(tags-todo
    "FOCUS"
    ((org-agenda-overriding-header
      (concat "To focus on"
              (if vulpea-agenda-hide-scheduled-and-waiting-next-tasks
                  ""
                " (including WAITING and SCHEDULED tasks)")))
     (org-agenda-skip-function 'vulpea-agenda-skip-habits)
     (org-tags-match-list-sublevels t)
     (org-agenda-todo-ignore-scheduled
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-deadlines
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-with-date
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-tags-todo-honor-ignore-options t)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

;;;###autoload
(defconst vulpea-agenda-cmd-stuck-projects
  '(tags-todo
    "PROJECT-CANCELLED-HOLD/!"
    ((org-agenda-overriding-header "Stuck Projects")
     (org-agenda-skip-function 'vulpea-agenda-skip-non-stuck-projects)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

;;;###autoload
(defconst vulpea-agenda-cmd-projects
  '(tags-todo
    "PROJECT-HOLD"
    ((org-agenda-overriding-header (concat "Projects"))
     (org-tags-match-list-sublevels t)
     (org-agenda-skip-function 'vulpea-agenda-skip-non-projects)
     (org-agenda-tags-todo-honor-ignore-options t)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

;;;###autoload
(defconst vulpea-agenda-cmd-waiting
  '(tags-todo
    "-CANCELLED+WAITING-READING-FOCUS|+HOLD/!"
    ((org-agenda-overriding-header
      (concat "Waiting and Postponed Tasks"
              (if vulpea-agenda-hide-scheduled-and-waiting-next-tasks
                  ""
                " (including WAITING and SCHEDULED tasks)")))
     (org-agenda-skip-function 'vulpea-agenda-skip-non-tasks)
     (org-tags-match-list-sublevels nil)
     (org-agenda-todo-ignore-scheduled
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-deadlines
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks))))


;; Utilities to build agenda commands -- skip

;;;###autoload
(defun vulpea-agenda-skip-habits ()
  "Skip tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun vulpea-agenda-skip-non-projects ()
  "Skip trees that are not projects."
  (if (save-excursion (vulpea-agenda-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((vulpea-agenda-project-p)
            nil)
           ((and (vulpea-agenda-project-subtree-p)
                 (not (vulpea-agenda-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

;;;###autoload
(defun vulpea-agenda-skip-non-tasks ()
  "Show non-project tasks.

Skip project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion
                           (or (outline-next-heading)
                               (point-max)))))
      (cond
       ((vulpea-agenda-task-p)
        nil)
       (t
        next-headline)))))

;;;###autoload
(defun vulpea-agenda-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion
                           (or (outline-next-heading)
                               (point-max)))))
      (if (vulpea-agenda-project-p)
          (let* ((subtree-end (save-excursion
                                (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ TODO "
                                             subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))


;; Utilities to build agenda commands -- predicates

;;;###autoload
(defun vulpea-agenda-project-p ()
  "Return non-nil if the heading at point is a project.

Basically, it's any item with some todo keyword and tagged as
PROJECT."
  (let* ((comps (org-heading-components))
         (todo (nth 2 comps))
         (tags (split-string (or (nth 5 comps) "") ":")))
    (and (member todo org-todo-keywords-1)
         (member "PROJECT" tags))))

;;;###autoload
(defun vulpea-agenda-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.

Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (vulpea-agenda-find-project-task)
      (if (equal (point) task)
          nil
        t))))

;;;###autoload
(defun vulpea-agenda-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))


;; Utilities to build agenda commands -- search

;;;###autoload
(defun vulpea-agenda-find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion
                         (org-back-to-heading 'invisible-ok)
                         (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components))
                      org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))



;;;###autoload
(defun vulpea-agenda-category (size)
  "Get category of item at point for agenda.

Resulting string respects SIZE, either by chopping characters
from the right or adding extra spaces to the right.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category 24) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((title (or (car-safe (org-roam--extract-titles-title))
                    (org-get-category)
                    (if buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))
                      "")))
         (extra (- size (length title))))
    (if (< extra 0)
        (concat
         (substring title 0 (- size 3))
         "...")
      (concat title (make-string extra ?\ )))))



(provide 'lib-vulpea-agenda)
;;; lib-vulpea-agenda.el ends here
