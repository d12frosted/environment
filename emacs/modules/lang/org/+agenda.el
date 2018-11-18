;;; lang/org/+agenda.el -*- lexical-binding: t; -*-

(defvar +org-hide-scheduled-and-waiting-next-tasks t)

(setq org-agenda-files (list org-directory
                             +org-tasks-dir
                             +org-notes-dir))
(setq org-agenda-custom-commands
      `((" " "Agenda"
         (
          (tags-todo "notifications"
                     ((org-agenda-overriding-header "Notifications")
                      (org-agenda-skip-function '+org--skip-future-tasks)
                      (org-tags-match-list-sublevels 'indented)))
          (agenda "" ((org-agenda-span 'day)
                      (org-agenda-sorting-strategy
                       '(habit-down time-up category-keep todo-state-down priority-down))))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header
                       (concat "Next Tasks"
                               (if +org-hide-scheduled-and-waiting-next-tasks
                                   ""
                                 " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+org--skip-projects-and-habits)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-sorting-strategy
                       '(todo-state-down priority-down effort-up category-keep))))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED-READING/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function '+org--skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANCELLED-READING/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function '+org--skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-READING/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if +org-hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+org--skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-READING/!"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if +org-hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+org--skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED+WAITING-READING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if +org-hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+org--skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled +org-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +org-hide-scheduled-and-waiting-next-tasks)))
          (tags "-REFILE-READING/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function '+org--skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil)))
          )
         nil)
        ("r" "Reading List" tags-todo "READING"
         ((org-agenda-overriding-header "Reading List")
          (org-agenda-remove-tags t)
          (org-agenda-prefix-format
           '((tags . "%(orgability-agenda-list-topics)")
             ))
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        ))

;;
;; Helpers

(defun +org/agenda-redo ()
  (interactive)
  (when org-agenda-buffer
    (with-current-buffer org-agenda-buffer
      (org-agenda-maybe-redo)
      (message "%s refreshed!" org-agenda-buffer-name))))

(defun +org/toggle-next-task-display ()
  (interactive)
  (setq +org-hide-scheduled-and-waiting-next-tasks (not +org-hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if +org-hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

;;
;; Filters

(defun +org--find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun +org--is-project-p ()
  "Any task with a todo keyword subtask."
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

(defun +org--is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.

Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (+org--find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun +org--is-task-p ()
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

(defun +org--is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun +org--list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.

  This is normally used by skipping functions where this variable
  is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun +org--list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.

  This is normally used by skipping functions where this variable
  is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun +org--skip-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (+org--is-project-p)
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

(defun +org--skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  ;; (+org--list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (+org--is-project-p)
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

(defun +org--skip-non-projects ()
  "Skip trees that are not projects."
  ;; (+org--list-sublevels-for-projects-indented)
  (if (save-excursion (+org--skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((+org--is-project-p)
            nil)
           ((and (+org--is-project-subtree-p) (not (+org--is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun +org--skip-non-tasks ()
  "Show non-project tasks.

Skip project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((+org--is-task-p)
        nil)
       (t
        next-headline)))))

(defun +org--skip-project-trees-and-habits ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+org--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun +org--skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single
non-project tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and +org-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((+org--is-project-p)
        next-headline)
       ((and (+org--is-task-p) (not (+org--is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun +org--skip-project-tasks-maybe ()
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
       ((+org--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (+org--is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (+org--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun +org--skip-project-tasks ()
  "Show non-project tasks.

Skip project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+org--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((+org--is-project-subtree-p)
        subtree-end)
       ((member (org-get-todo-state) (list "NEXT"))
        subtree-end)
       (t
        nil)))))

(defun +org--skip-non-project-tasks ()
  "Show project tasks.

Skip project and sub-project tasks, habits, and loose non-project
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((+org--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (+org--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (+org--is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun +org--skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+org--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun +org--skip-non-subprojects ()
  "Skip trees that are not projects."
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (+org--is-subproject-p)
        nil
      next-headline)))

(defun +org--skip-non-archivable-tasks ()
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

(defun +org--skip-future-tasks ()
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
