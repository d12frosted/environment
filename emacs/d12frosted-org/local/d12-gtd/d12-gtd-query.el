;;; d12-gtd-query.el --- query helpers for personal GTD implementation

;; Copyright (c) 2015-2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 09 Jan 2017

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ((org))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; Mostly based on http://doc.norang.ca/org-mode.html

;;; Code:
;;

(require 'd12-gtd-time)

(defun d12-gtd-query/entry-get (f pom property &optional inherit literal-nil)
  "Proxy for `org-entry-get' that applies F to result."
  (when-let ((val (org-entry-get pom property inherit literal-nil)))
    (funcall f val)))

(defun d12-gtd-query/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun d12-gtd-query/is-project-p ()
  "Any task with a todo keyword subtask"
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
      (and is-a-task has-subtask))))

(defun d12-gtd-query/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (d12-gtd-query/find-project-task)
      (not (equal (point) task)))))

(defun d12-gtd-query/is-task-p ()
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

(defun d12-gtd-query/is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun d12-gtd-query/is-non-stuck-project-p ()
  "Any project with 'NEXT' task."
  (when (d12-gtd-query/is-project-p)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (has-next))
      (save-excursion
        (forward-line 1)
        (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
          (unless (member "WAITING" (org-get-tags-at))
            (setq has-next t))))
      has-next)))

(defun d12-gtd-query/is-stuck-project-p ()
  "Any project without 'NEXT' task."
  (when (d12-gtd-query/is-project-p)
    (not (d12-gtd-query/is-non-stuck-project-p))))

(defun d12-gtd-query/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun d12-gtd-query/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar d12-gtd-query/hide-scheduled-and-waiting-next-tasks t)

(defun d12-gtd-query/toggle-next-task-display ()
  (interactive)
  (setq d12-gtd-query/hide-scheduled-and-waiting-next-tasks (not d12-gtd-query/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if d12-gtd-query/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun d12-gtd-query/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (and (d12-gtd-query/is-project-p)
               (d12-gtd-query/is-non-stuck-project-p))
          nil
        next-headline))))

(defun d12-gtd-query/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (and (d12-gtd-query/is-project-p)
               (d12-gtd-query/is-stuck-project-p))
          nil
        next-headline))))

(defun d12-gtd-query/skip-non-projects ()
  "Skip trees that are not projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (d12-gtd-query/is-project-p)
          nil
        next-headline)))
  ;; (if (save-excursion (d12-gtd-query/skip-non-stuck-projects))
  ;;     (save-restriction
  ;;       (widen)
  ;;       (let ((subtree-end (save-excursion (org-end-of-subtree t))))
  ;;         (cond
  ;;          ((d12-gtd-query/is-project-p)
  ;;           nil)
  ;;          ((and (d12-gtd-query/is-project-subtree-p) (not (d12-gtd-query/is-task-p)))
  ;;           nil)
  ;;          (t
  ;;           subtree-end))))
  ;;   (save-excursion (org-end-of-subtree t)))
  )

(defun d12-gtd-query/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((d12-gtd-query/is-task-p)
        nil)
       (t
        next-headline)))))

(defun d12-gtd-query/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((d12-gtd-query/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun d12-gtd-query/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and d12-gtd-query/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((d12-gtd-query/is-project-p)
        next-headline)
       ((and (d12-gtd-query/is-task-p) (not (d12-gtd-query/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun d12-gtd-query/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((d12-gtd-query/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (d12-gtd-query/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (d12-gtd-query/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun d12-gtd-query/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((d12-gtd-query/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((d12-gtd-query/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun d12-gtd-query/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((d12-gtd-query/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (d12-gtd-query/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (d12-gtd-query/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun d12-gtd-query/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((d12-gtd-query/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun d12-gtd-query/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (d12-gtd-query/is-subproject-p)
        nil
      next-headline)))

(defun d12-gtd-query/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
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

(defun d12-gtd-query/completed-at (time)
  (let ((next-headline (save-excursion (outline-next-heading)))
        (ctime (d12-gtd-query/entry-get #'d12-gtd-time-parse  (point) "CLOSED")))
    (if (and ctime (d12-gtd-date-eq time ctime))
        nil
      next-headline)))

(defun d12-gtd-query/completed-between (time0 time1)
  (let ((next-headline (save-excursion (outline-next-heading)))
        (ctime (d12-gtd-query/entry-get  #'d12-gtd-time-parse  (point) "CLOSED")))
    (if (and ctime
             (d12-gtd-date-lte time0 ctime)
             (d12-gtd-date-gte time1 ctime))
        nil
      next-headline)))

(provide 'd12-gtd-query)

;;; d12-gtd-query.el ends here
