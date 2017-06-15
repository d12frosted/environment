;;; d12-gtd-agendablock.el --- agendablocks for personal GTD implementation

;; Copyright (c) 2015-2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 09 Jan 2017

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(require 'd12-gtd-query)

(defun d12-gtd-agendablock/completed (name day-span &optional files)
  `(todo ,(mapconcat #'identity '("DONE" "CANCELLED" "MEETING") "|")
         ((org-agenda-overriding-header ,(format "Completed %s" name))
          (org-agenda-archives-mode t)
          (org-agenda-skip-function
           '(d12-gtd-query/completed-at (time-add (current-time) (seconds-to-time ,(* 24 60 60 day-span))))
           )
          (org-agenda-files
           (if ',files
               ',(mapcar #'d12-gtd/get-file-path files)
             org-agenda-files)))))

(defun d12-gtd-agendablock/completed-between (name span0 span1 &optional files)
  `(todo ,(mapconcat #'identity '("DONE" "CANCELLED" "MEETING") "|")
         ((org-agenda-overriding-header ,(format "Completed %s" name))
          (org-agenda-archives-mode t)
          (org-agenda-skip-function
           '(d12-gtd-query/completed-between (time-add (current-time) (seconds-to-time ,(* 24 60 60 span0)))
                                             (time-add (current-time) (seconds-to-time ,(* 24 60 60 span1))))
           )
          (org-agenda-files
           (if ',files
               ',(mapcar #'d12-gtd/get-file-path files)
             org-agenda-files)))))

(defun d12-gtd-agendablock/today (&optional files)
  `(agenda ""
           ((org-agenda-span 'day)
            (org-agenda-files
             (if ',files
                 ',(mapcar #'d12-gtd/get-file-path files)
               org-agenda-files)))))

(defun d12-gtd-agendablock/refile ()
  `(todo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-tags-match-list-sublevels nil)
          (org-agenda-files '(,(d12-gtd/get-file-path "inbox"))))))

(defun d12-gtd-agendablock/stuck-projects (&optional files)
  `(tags-todo "-CANCELLED/!"
              ((org-agenda-overriding-header "Stuck Projects")
               (org-agenda-skip-function 'd12-gtd-query/skip-non-stuck-projects)
               (org-agenda-sorting-strategy
                '(category-keep))
               (org-agenda-files
                (if ',files
                    ',(mapcar #'d12-gtd/get-file-path files)
                  org-agenda-files)))))

(defun d12-gtd-agendablock/projects (&optional files)
  `(tags-todo "-HOLD-CANCELLED/!"
              ((org-agenda-overriding-header "Projects")
               (org-agenda-skip-function 'd12-gtd-query/skip-stuck-projects)
               ;; (org-tags-match-list-sublevels 'indented)
               (org-agenda-sorting-strategy
                '(category-keep))
               (org-agenda-files
                (if ',files
                    ',(mapcar #'d12-gtd/get-file-path files)
                  org-agenda-files)))))

(defun d12-gtd-agendablock/next-tasks (&optional files)
  `(tags-todo "-CANCELLED/!NEXT"
              ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                     (if d12-gtd-query/hide-scheduled-and-waiting-next-tasks
                                                         ""
                                                       " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'd12-gtd-query/skip-projects-and-habits-and-single-tasks)
               (org-tags-match-list-sublevels t)
               (org-agenda-todo-ignore-scheduled d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-deadlines d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-with-date d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy
                '(todo-state-down effort-up category-keep))
               (org-agenda-files
                (if ',files
                    ',(mapcar #'d12-gtd/get-file-path files)
                  org-agenda-files)))))

(defun d12-gtd-agendablock/project-substasks (&optional files)
  `(tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
              ((org-agenda-overriding-header
                (concat "Project Subtasks"
                        (if d12-gtd-query/hide-scheduled-and-waiting-next-tasks
                            ""
                          " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'd12-gtd-query/skip-non-project-tasks)
               (org-agenda-todo-ignore-scheduled d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-deadlines d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-with-date d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy
                '(category-keep))
               (org-agenda-files
                (if ',files
                    ',(mapcar #'d12-gtd/get-file-path files)
                  org-agenda-files)))))

(defun d12-gtd-agendablock/standalone-tasks (&optional files)
  `(tags-todo "-CANCELLED-WAITING-HOLD/!"
              ((org-agenda-overriding-header
                (concat "Standalone Tasks"
                        (if d12-gtd-query/hide-scheduled-and-waiting-next-tasks
                            ""
                          " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'd12-gtd-query/skip-project-tasks)
               (org-agenda-todo-ignore-scheduled d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-deadlines d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-with-date d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-sorting-strategy
                '(category-keep))
               (org-agenda-files
                (if ',files
                    ',(mapcar #'d12-gtd/get-file-path files)
                  org-agenda-files)))))

(defun d12-gtd-agendablock/postponed-tasks (&optional files)
  `(tags-todo "-CANCELLED+WAITING|HOLD/!"
              ((org-agenda-overriding-header
                (concat "Waiting and Postponed Tasks"
                        (if d12-gtd-query/hide-scheduled-and-waiting-next-tasks
                            ""
                          " (including WAITING and SCHEDULED tasks)")))
               (org-agenda-skip-function 'd12-gtd-query/skip-non-tasks)
               (org-tags-match-list-sublevels nil)
               (org-agenda-todo-ignore-scheduled d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-todo-ignore-deadlines d12-gtd-query/hide-scheduled-and-waiting-next-tasks)
               (org-agenda-files
                (if ',files
                    ',(mapcar #'d12-gtd/get-file-path files)
                  org-agenda-files)))))

(defun d12-gtd-agendablock/tasks-to-archive (&optional files)
  `(tags "-REFILE/"
         ((org-agenda-overriding-header "Tasks to Archive")
          (org-agenda-skip-function 'd12-gtd-query/skip-non-archivable-tasks)
          (org-tags-match-list-sublevels nil)
          (org-agenda-files
           (if ',files
               ',(mapcar #'d12-gtd/get-file-path files)
             org-agenda-files)))))

(provide 'd12-gtd-agendablock)

;;; d12-gtd-agendablock.el ends here
