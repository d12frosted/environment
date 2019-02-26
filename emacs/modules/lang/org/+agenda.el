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
  (setq
   org-agenda-files (list org-directory
			                    (concat org-directory "notes/")
			                    (concat org-directory "gtd/"))
	 ;; also show state change in log mode
	 org-agenda-log-mode-items '(closed clock state)

	 ;; setup archive
	 org-archive-location (concat org-directory ".archive/archive" "::")
	 org-archive-save-context-info '(time file ltags itags todo category olpath)

	 ;; more structured view
	 org-agenda-prefix-format
   '((agenda . " %i %-24:c%?-12t% s")
     (todo . " %i %-24:c")
     (tags . " %i %-24:c")
     (search . " %i %-24:c"))
	 org-agenda-todo-keyword-format "%-1s"
	 org-agenda-tags-column 0

   ;; show agenda in current window
   org-agenda-window-setup 'current-window))

(setq org-agenda-custom-commands
  `((" " "Agenda"
     (
      (tags "REFILE"
            ((org-agenda-overriding-header "To refile")
             (org-tags-match-list-sublevels nil)))
      (agenda "" ((org-agenda-span 'day)
                  (org-agenda-sorting-strategy
                   '(habit-down time-up category-keep todo-state-down priority-down))))
      (tags-todo "FOCUS"
                 ((org-agenda-overriding-header
                   (concat "To focus on"
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
      (tags "-REFILE-READING/"
            ((org-agenda-overriding-header "To archive")
             (org-agenda-skip-function '+agenda--skip-non-archivable-tasks)
             (org-tags-match-list-sublevels nil)))
      (tags-todo "-CANCELLED+WAITING-READING-focus|HOLD/!"
                 ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                        (if +agenda-hide-scheduled-and-waiting-next-tasks
                                                            ""
                                                          " (including WAITING and SCHEDULED tasks)")))
                  (org-agenda-skip-function '+agenda--skip-non-tasks)
                  (org-tags-match-list-sublevels nil)
                  (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
                  (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)))
      )
     ((org-agenda-buffer-name +agenda-main-buffer-name)))
    ("r" "Reading List" tags-todo "READING"
     ((org-agenda-overriding-header "Reading List")
      (org-agenda-remove-tags t)
      (org-agenda-prefix-format
       '((tags . "%(orgability-agenda-list-topics)")
         ))
      (org-agenda-sorting-strategy
       '(todo-state-down effort-up category-keep))
      (org-agenda-buffer-name +agenda-reading-list-buffer-name)))
    ("h" "Habits" tags-todo "STYLE=\"habit\""
     ((org-agenda-overriding-header "Habits")
      (org-agenda-sorting-strategy
       '(todo-state-down effort-up category-keep))
      (org-agenda-buffer-name +agenda-habits-buffer-name)))
    ))
