;;; +org-agenda.el --- utilities for building agenda -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'subr-x)
(require 'init-buffer)
(require '+org)

(defvar org-directory)
(defvar org-agenda-files)

;;;###autoload
(defvar +agenda-hide-scheduled-and-waiting-next-tasks t)

;;;###autoload
(defvar +agenda-main-buffer-name "*agenda:main*"
  "Name of the main agenda buffer.")

;;;###autoload
(defvar +agenda-main-cache-buffer nil
  "When non-nil `+agenda/main' will cache the buffer.")

;;;###autoload
(defvar +agenda-reading-list-buffer-name "*agenda:reading-list*"
  "Name of the reading list agenda buffer.")

;;;###autoload
(defvar +agenda-habits-buffer-name "*agenda:habits*"
  "Name of the habits agenda buffer.")

(defun +agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (+org-notes-project-files)))

;;;###autoload
(defun +agenda/main ()
  "Show main `org-agenda' view."
  (interactive)
  (org-agenda nil " "))

;;;###autoload
(defun +agenda/person ()
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
         (tags (seq-map #'+org-notes--title-to-tag names))
         (query (string-join tags "|")))
    (dlet ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))

(defconst +agenda--refile
  '(tags "REFILE"
         ((org-agenda-overriding-header "To refile")
          (org-tags-match-list-sublevels nil))))

(defconst +agenda--today
  '(agenda ""
           ((org-agenda-span 'day)
		        (org-agenda-skip-deadline-prewarning-if-scheduled t)
		        (org-agenda-sorting-strategy
		         '(habit-down time-up category-keep todo-state-down priority-down)))))

(defconst +agenda--stuck-projects
  '(tags-todo "PROJECT-CANCELLED-HOLD/!"
              ((org-agenda-overriding-header "Stuck Projects")
               (org-agenda-skip-function '+agenda--skip-non-stuck-projects)
               (org-agenda-sorting-strategy
                '(todo-state-down priority-down effort-up category-keep)))))

(defconst +agenda--projects
  '(tags-todo "PROJECT-HOLD"
              ((org-agenda-overriding-header (concat "Projects"))
               (org-tags-match-list-sublevels t)
               (org-agenda-skip-function '+agenda--skip-non-projects)
               (org-agenda-tags-todo-honor-ignore-options t)
               (org-agenda-sorting-strategy
                '(todo-state-down priority-down effort-up category-keep)))))

(defconst +agenda--project-subtasks
  '(tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
              ((org-agenda-overriding-header
                (concat "Project Subtasks"
                        (if +agenda-hide-scheduled-and-waiting-next-tasks
                            ""
                          " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function '+agenda--skip-non-project-tasks)
               (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-with-date +agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy
                '(todo-state-down priority-down effort-up category-keep)))))

(defconst +agenda--focus
  '(tags-todo "FOCUS"
              ((org-agenda-overriding-header
                (concat "To focus on"
                        (if +agenda-hide-scheduled-and-waiting-next-tasks
                            ""
                          " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function '+agenda--skip-habits)
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-with-date +agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-tags-todo-honor-ignore-options t)
               (org-agenda-sorting-strategy
                '(todo-state-down priority-down effort-up category-keep)))))

(defconst +agenda--archive
  '(tags "-REFILE-READING/"
         ((org-agenda-overriding-header "To archive")
          (org-agenda-skip-function '+agenda--skip-non-archivable-tasks)
          (org-tags-match-list-sublevels nil))))

(defconst +agenda--waiting
  '(tags-todo "-CANCELLED+WAITING-READING-FOCUS|+HOLD/!"
              ((org-agenda-overriding-header
                (concat "Waiting and Postponed Tasks"
                        (if +agenda-hide-scheduled-and-waiting-next-tasks
                            ""
                          " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function '+agenda--skip-non-tasks)
               (org-tags-match-list-sublevels nil)
               (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks))))

;;;###autoload
(defun +agenda--find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;;;###autoload
(defun +agenda--is-project-p ()
  "Return non-nil if the heading at point is a project.

Basically, it's any item with some todo keyword and tagged as
PROJECT."
  (let* ((comps (org-heading-components))
         (todo (nth 2 comps))
         (tags (split-string (or (nth 5 comps) "") ":")))
    (and (member todo org-todo-keywords-1)
         (member "PROJECT" tags))))

;;;###autoload
(defun +agenda--is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.

Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (+agenda--find-project-task)
      (if (equal (point) task)
          nil
        t))))

;;;###autoload
(defun +agenda--is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

;;;###autoload
(defun +agenda--is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

;;;###autoload
(defun +agenda--list-sublevels-for-projects-indented ()
  "Make all subtasks visible during subtree restriction.

Sets `org-tags-match-list-sublevels' so when restricted to a
subtree we list all subtasks.

This is normally used by skipping functions where this variable
is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

;;;###autoload
(defun +agenda--list-sublevels-for-projects ()
  "Make all subtasks visible during subtree restriction.

Sets `org-tags-match-list-sublevels' so when restricted to a
subtree we list all subtasks.

This is normally used by skipping functions where this variable
is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

;;;###autoload
(defun +agenda/toggle-next-task-display ()
  "Toggle displaying of next tasks in agenda."
  (interactive)
  (setq +agenda-hide-scheduled-and-waiting-next-tasks (not +agenda-hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if +agenda-hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

;;;###autoload
(defun +agenda--skip-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (+agenda--is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

;;;###autoload
(defun +agenda--skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  ;; (+agenda--list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (+agenda--is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ TODO " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;;;###autoload
(defun +agenda--skip-non-projects ()
  "Skip trees that are not projects."
  ;; (+agenda--list-sublevels-for-projects-indented)
  (if (save-excursion (+agenda--skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((+agenda--is-project-p)
            nil)
           ((and (+agenda--is-project-subtree-p) (not (+agenda--is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

;;;###autoload
(defun +agenda--skip-non-tasks ()
  "Show non-project tasks.

Skip project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((+agenda--is-task-p)
        nil)
       (t
        next-headline)))))

;;;###autoload
(defun +agenda--skip-project-trees-and-habits ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+agenda--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +agenda--skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, habits or single tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and +agenda-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags)))
        next-headline)
       ((+agenda--is-project-p)
        next-headline)
       ((and (+agenda--is-task-p) (not (+agenda--is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

;;;###autoload
(defun +agenda--skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.

When restricted to a project, skip project and sub project tasks,
habits, and loose tasks. When not restricted, skip project and
sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((+agenda--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (+agenda--is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (+agenda--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +agenda--skip-project-tasks ()
  "Show non-project tasks.

Skip project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+agenda--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((+agenda--is-project-subtree-p)
        subtree-end)
       ((member (org-get-todo-state) (list "NEXT"))
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +agenda--skip-non-project-tasks ()
  "Show project tasks.

Skip project and sub-project tasks, habits, and loose non-project
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((+agenda--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (+agenda--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (+agenda--is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +agenda--skip-habits ()
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
(defun +agenda--skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+agenda--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +agenda--skip-non-subprojects ()
  "Skip trees that are not projects."
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (+agenda--is-subproject-p)
        nil
      next-headline)))

;;;###autoload
(defun +agenda--skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving."
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

;;;###autoload
(defun +agenda--skip-future-tasks ()
  "Skip trees that are scheduled in the future."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-seconds
           (time-to-seconds
            (org-time-string-to-time
             (org-entry-get nil "SCHEDULED"))))
          (now (time-to-seconds (current-time))))
      (and scheduled-seconds
           (>= scheduled-seconds now)
           subtree-end))))

(provide '+org-agenda)
;;; +org-agenda.el ends here
