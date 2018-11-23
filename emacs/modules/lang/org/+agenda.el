;;; lang/org/+agenda.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(add-hook 'org-load-hook #'+org|setup-agenda)

(defun +org|setup-agenda ()
  "Setup `org-agenda'."
  (setq org-agenda-files (list org-directory
			       (concat org-directory "notes/")
			       (concat org-directory "gtd/"))
	;; also show state change in log mode
	org-agenda-log-mode-items '(closed clock state)

	;; setup archive
	org-archive-location (concat org-directory ".archive/archive" "::")
	org-archive-save-context-info '(time file ltags itags todo category olpath)))

(setq org-agenda-custom-commands
      `((" " "Agenda"
         (
          (tags-todo "notifications"
                     ((org-agenda-overriding-header "Notifications")
                      (org-agenda-skip-function '+agenda--skip-future-tasks)
                      (org-tags-match-list-sublevels 'indented)))
          (agenda "" ((org-agenda-span 'day)
                      (org-agenda-sorting-strategy
                       '(habit-down time-up category-keep todo-state-down priority-down))))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header
                       (concat "Next Tasks"
                               (if +agenda-hide-scheduled-and-waiting-next-tasks
                                   ""
                                 " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+agenda--skip-projects-and-habits)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-sorting-strategy
                       '(todo-state-down priority-down effort-up category-keep))))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED-READING/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function '+agenda--skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANCELLED-READING/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function '+agenda--skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-READING/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if +agenda-hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+agenda--skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-READING/!"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if +agenda-hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+agenda--skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED+WAITING-READING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if +agenda-hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function '+agenda--skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)))
	  (tags "-REFILE-READING/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function '+agenda--skip-non-archivable-tasks)
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
