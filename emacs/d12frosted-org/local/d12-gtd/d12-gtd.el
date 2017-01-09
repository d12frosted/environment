;;; d12-gtd.el --- personal GTD implementation

;; Copyright (c) 2017 Boris Buliga

;; Author: Boris Buliga <d12frosted@gmail.com>
;; Maintainer: Boris Buliga <d12frosted@gmail.com>
;; Created: 09 Jan 2017

;; Keywords: org-mode
;; Homepage: https://github.com/d12frosted/environment/tree/master/emacs/d12frosted-org/local/d12-gtd

;; Package-Version: 0.0.1
;; Package-Requires: ((org) (org-query))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; Note that this file implicitly depends on d12-path variables (that are currently defined in d12-init.el)

;;; Code:
;;

;; require org packages
(require 'org)
(require 'org-habit)

;; require helpers
(require 'd12-gtd-time)
(require 'd12-gtd-query)
(require 'd12-gtd-agendablock)

;; require org-query
(require 'org-query)
(require 'org-query-gtd)

;; Define functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun d12-gtd/get-file-path (name)
  "Return path to org file with NAME."
  (format "%s%s.org" d12-path/org-home name))

(defun d12-gtd/reload-agenda-files ()
  "Reload agenda files. Useful for situations when file is added
or removed from org files directory."
  (interactive)
  (setq org-agenda-files (d12-files/query "*.org" d12-path/org-home 2))
  (when (functionp #'d12-interesting-files-add)
    (d12-interesting-files-add org-agenda-files)))

;; Configure org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup location of org files
(setq org-directory d12-path/org-home)
(setq org-default-notes-file (d12-gtd/get-file-path "notes"))
(setq org-archive-location "archive/%s_archive::")
(d12-gtd/reload-agenda-files)

;; Setup todo keywords and triggers
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "MEETING")))
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        (done ("WAITING"))
        ("TODO" ("WAITING") ("CANCELLED"))
        ("NEXT" ("WAITING") ("CANCELLED"))
        ("DONE" ("WAITING") ("CANCELLED"))))

;; Formatting
(setq org-agenda-prefix-format
      '((agenda . " %i %-24:c %-12t %-12s ")
        (timeline . "  % s")
        (todo . " %i %-24:c")
        (tags . " %i %-24:c")
        (search . " %i %-24:c")))
(setq org-property-format "%-16s %s")
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Configure refile targets
(setq org-refile-targets
      (quote ((nil :maxlevel . 9)
              (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-indirect-buffer-display 'current-window)

;; Sorting
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo category-keep todo-state-up priority-down)
        (tags category-keep todo-state-up priority-down)
        (search category-keep todo-state-up priority-down)))

;; Agenda
(setq org-agenda-custom-commands
      `(

        ;; Global queries
        ("g" . "Global")
        ("gr" "Review"
         (
          ,(d12-gtd-agendablock/completed "Yesterday" -1)
          ,(d12-gtd-agendablock/completed "Today" 0)
          ,(d12-gtd-agendablock/today))
         nil
         ("~/Dropbox/agenda/Global - Review.html"))

        ;; Personal queries
        ("p" . "Personal")
        ("pr" "Review"
         (
          ,(d12-gtd-agendablock/completed "Yesterday" -1 '("personal"))
          ,(d12-gtd-agendablock/completed "Today" 0 '("personal"))
          ,(d12-gtd-agendablock/today '("personal")))
         nil
         ("~/Dropbox/agenda/Personal - Review.html"))

        ;; d12frosted queries
        ("d" . "d12frosted")
        ("dr" "Review"
         (
          ,(d12-gtd-agendablock/completed "Yesterday" -1 '("d12frosted"))
          ,(d12-gtd-agendablock/completed "Today" 0 '("d12frosted"))
          ,(d12-gtd-agendablock/today '("d12frosted")))
         nil
         ("~/Dropbox/agenda/d12frosted - Review.html"))

        ;; Work queries
        ("w" . "Work")
        ("wr" "Review"
         (
          ,(d12-gtd-agendablock/completed "Yesterday" -1 '("work"))
          ,(d12-gtd-agendablock/completed "Today" 0 '("work"))
          ,(d12-gtd-agendablock/today '("work")))
         nil
         ("~/Dropbox/agenda/Work - Review.html"))

        (" " "Agenda"
         (
          ,(d12-gtd-agendablock/today)
          ,(d12-gtd-agendablock/refile)
          ,(d12-gtd-agendablock/stuck-projects)
          ,(d12-gtd-agendablock/projects)
          ,(d12-gtd-agendablock/next-tasks)
          ,(d12-gtd-agendablock/project-substasks)
          ,(d12-gtd-agendablock/standalone-tasks)
          ,(d12-gtd-agendablock/postponed-tasks)
          ,(d12-gtd-agendablock/tasks-to-archive)
          )
         nil)

        ))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)

;; Agenda export settings
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
        (ps-landscape-mode t)
        (org-agenda-add-entry-text-maxlines 5)
        (htmlize-output-type 'css)))

;; Capture settings
(setq org-capture-templates
      `(("t" "todo" plain (file ,(d12-gtd/get-file-path "inbox"))
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file ,(d12-gtd/get-file-path "inbox"))
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file ,(d12-gtd/get-file-path "inbox"))
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file ,(d12-gtd/get-file-path "inbox"))
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree+prompt ,(d12-gtd/get-file-path "journal"))
         "* %?\n%U\n" :clock-in t :clock-resume t)))

;; MobileOrg
(setq org-mobile-directory (concat d12-path/dropbox "Apps/d12-mobile-org"))
(setq org-mobile-inbox-for-pull (d12-gtd/get-file-path "inbox"))

;; Use C-c C-t for state change (when worf is not available)
(setq org-use-fast-todo-selection t)

;; For a quick state fix (without state change query or time stamping)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Other settings
(setq org-agenda-inhibit-startup nil)

;; Provide package
(provide 'd12-gtd)

;;; d12-gtd.el ends here
