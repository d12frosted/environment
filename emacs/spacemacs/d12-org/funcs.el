;;; funcs.el --- d12-org layer funcs file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; Most of the functions defined here are renamed functions from
;; http://doc.norang.ca/org-mode.html
;;
;;; Code:

(defun d12-org/mobile-sync ()
  "Sync org-mobile."
  (interactive)
  (org-mobile-pull)
  (org-mobile-push))

(defun d12-org/mobile-sync-enable ()
  "Enable org-mobile sync."
  (interactive)
  (d12-org/mobile-sync-disable)
  (setq d12-org--mobile-sync-timer
        (run-with-idle-timer
         d12-org/mobile-sync-idle-secs t
         'd12-org/mobile-sync)))

(defun d12-org/mobile-sync-disable ()
  "Disable org-mobile sync."
  (interactive)
  (when d12-org--mobile-sync-timer
    (cancel-timer d12-org--mobile-sync-timer)
    (setq d12-org--mobile-sync-timer nil)))

(defun d12-org/agenda-redo ()
  (interactive)
  (when-let ((name "*Org Agenda*")
             (_ (get-buffer name)))
    (with-current-buffer name
      (org-agenda-maybe-redo)
      (message "[org agenda] refreshed!"))))

(defun d12-org/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun d12-org--find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun d12-org--is-project-p ()
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

(defun d12-org--is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (d12-org--find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun d12-org--is-task-p ()
  "Any task with a todo keyword and no subtask"
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

(defun d12-org--is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun d12-org--list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun d12-org--list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun d12-org/toggle-next-task-display ()
  (interactive)
  (setq d12-org-hide-scheduled-and-waiting-next-tasks (not d12-org-hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if d12-org-hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun d12-org--skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (d12-org--is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun d12-org--skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (d12-org--list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (d12-org--is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun d12-org--skip-non-projects ()
  "Skip trees that are not projects"
  ;; (d12-org--list-sublevels-for-projects-indented)
  (if (save-excursion (d12-org--skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((d12-org--is-project-p)
            nil)
           ((and (d12-org--is-project-subtree-p) (not (d12-org--is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun d12-org--skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((d12-org--is-task-p)
        nil)
       (t
        next-headline)))))

(defun d12-org--skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((d12-org--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun d12-org--skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and d12-org-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((d12-org--is-project-p)
        next-headline)
       ((and (d12-org--is-task-p) (not (d12-org--is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun d12-org--skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((d12-org--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (d12-org--is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (d12-org--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun d12-org--skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((d12-org--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((d12-org--is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun d12-org--skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((d12-org--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (d12-org--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (d12-org--is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun d12-org--skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((d12-org--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun d12-org--skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (d12-org--is-subproject-p)
        nil
      next-headline)))

(defun d12-org--skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
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

(defun d12-org/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(defun d12-org/yank-markdown ()
  "Insert converted markdown from macOS clipboard to org-mode."
  (interactive)
  (insert
   (shell-command-to-string
    "pbpaste | pandoc --wrap=none --from=markdown --to=org")))

(defun d12-org/yank-html ()
  "Insert converted html from macOS clipboard to org-mode."
  (interactive)
  (insert
   (shell-command-to-string
    "pbpaste | pandoc --wrap=none --from=html --to=org")))

(defun d12-org/enable-latex-preview ()
  (interactive)
  (org-toggle-latex-fragment '(16)))

;;; funcs.el ends here
