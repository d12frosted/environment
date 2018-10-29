;;; packages.el --- d12-org layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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
;;; Code:

(defconst d12-org-packages
  '((org-plus-contrib :step pre)
    org
    worf
    (ocu :location (recipe :fetcher local))
    (d12-auto-id :location (recipe :fetcher local))
    org-brain
    persp-mode
    toc-org
    (orgability :location built-in)
    org-board
    org-download
    org-web-tools
    )
  "The list of Lisp packages required by the d12-org layer.")


;; dummy init function to force installation of `org-plus-contrib'
(defun d12-org/init-org-plus-contrib ())

(defun d12-org/init-org ()
  (use-package org
    :commands (orgtbl-mode org-mobile-push org-mobile-pull)
    :init
    ;; Setup directory for all org files
    (setq org-directory d12-path-org-home)

    ;; Setup useful global key bindings.
    (spacemacs/declare-prefix "oo" "org")
    (d12-key-bind-personal "c" 'org-capture)
    (d12-key-bind-personal "ob" 'org-iswitchb)
    (d12-key-bind-personal "ol" 'org-store-link)
    (d12-key-bind-personal "og" 'org-clock-goto)
    (d12-key-bind "C-<f11>" 'org-clock-in)
    (d12-key-bind "C-<f12>" 'org-clock-out)
    (d12-key-bind "<f9> c" 'calendar)
    (d12-key-bind "<f9> h" 'd12-org/hide-other)
    (d12-key-bind "<f9> i" 'org-toggle-inline-images)
    (d12-key-bind "<f9> l" 'org-toggle-link-display)
    (d12-key-bind "<f9> v" 'visible-mode)
    (d12-key-bind "<f12>" 'org-agenda)

    ;; Setup indent mode
    (setq org-startup-indented t)

    ;; Setup org-mobile
    (setq org-mobile-directory (concat d12-path-dropbox "Apps/d12-mobile-org/"))
    (setq org-mobile-inbox-for-pull (d12-path/get-org-file "mobile"))

    ;; Setup `org-agenda-files'. Prefer using directories as files add to them
    ;; will automatically get into `org-agenda-files'.
    (setq org-agenda-files `(,d12-path-org-home
                             ,d12-path-org-notes-home
                             ,d12-path-org-tasks-home))

    ;; Setup `org-todo-keywords'.
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

    ;; Enable fast todo selection.
    (setq org-use-fast-todo-selection t)

    ;; Allow to fast fix todo state without triggering anything.
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)

    ;; Setup state triggers.
    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

    ;; refresh agenda after capturing
    (add-hook 'org-capture-after-finalize-hook #'d12-org/agenda-redo)

    ;; Setup capture templates.
    (setq org-capture-templates
          `(("t" "todo" plain (file ,(d12-path/get-org-file "inbox"))
             "* TODO %?\n%U\n" :clock-in t :clock-resume t)
            ("T" "todo (annotated)" plain (file ,(d12-path/get-org-file "inbox"))
             "* TODO %?\n%U%a\n" :clock-in t :clock-resume t)

            ("j" "Journal" entry (file+datetree+prompt ,(d12-path/get-org-note-file "journal"))
             "* %?\n%U\n" :clock-in t :clock-resume t)

            ("r" "respond" entry (file ,(d12-path/get-org-file "inbox"))
             "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)

            ("n" "note" entry (file ,(d12-path/get-org-file "inbox"))
             "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

            ("m" "Meeting" entry (file ,(d12-path/get-org-file "inbox"))
             "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
            ("h" "Habit" entry (file ,(d12-path/get-org-file "inbox"))
             "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

    ;; remove clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Remove empty drawers on clock out
    (add-hook 'org-clock-out-hook 'd12-org/remove-empty-drawer-on-clock-out 'append)

    ;; Refiling
    (setq org-refile-targets
          '((nil :maxlevel . 9)
            (org-agenda-files :maxlevel . 9)))
    (setq org-refile-use-outline-path t)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-indirect-buffer-display 'current-window)
    (defun d12-org--verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (let ((tags-at (org-get-tags-at)))
        (and
         ;; doesn't have done keyword
         (not (member (nth 2 (org-heading-components)) org-done-keywords))

         ;; doesn't have blacklisted tag
         (or (null tags-at)
             (member-if-not
              (lambda (x)
                (member (if (listp x) (car x) x) d12-org-refile-ignore-tags))
              tags-at)))))
    (setq org-refile-target-verify-function 'd12-org--verify-refile-target)

    ;; Do not dim blocked tasks
    (setq org-agenda-dim-blocked-tasks nil)

    ;; Compact the block agenda view
    (setq org-agenda-compact-blocks t)

    ;; also show state change in log mode
    (setq org-agenda-log-mode-items '(closed clock state))

    ;; use drawer for state changes
    (setq org-log-into-drawer t)

    ;; When inhibiting org-agenda, show it full screen
    (setq org-agenda-window-setup 'only-window)

    ;; tune visuals of agenda
    (setq org-agenda-block-separator "")
    (setq org-agenda-compact-blocks nil)

    ;; better image inlining
    (setq org-image-actual-width 512)

    ;; setup attachment directory
    (setq org-attach-directory ".data")

    ;; setup 'cache' directories
    (setq org-preview-latex-image-directory ".ltximg/")
    (setq org-preview-latex-default-process 'dvisvgm)

    ;; setup archive
    (setq org-archive-location (concat (d12-path/get-org-file ".archive/archive") "::"))
    (setq org-archive-save-context-info '(time file ltags itags todo category olpath))

    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          `((" " "Agenda"
             (
              (tags-todo "notifications"
                         ((org-agenda-overriding-header "Notifications")
                          (org-agenda-skip-function 'd12-org--skip-future-tasks)
                          (org-tags-match-list-sublevels 'indented)))
              (agenda "" ((org-agenda-span 'day)
                          (org-agenda-sorting-strategy
                           '(habit-down time-up category-keep todo-state-down priority-down))))
              (tags-todo "-CANCELLED/!NEXT"
                         ((org-agenda-overriding-header
                           (concat "Next Tasks"
                                   (if d12-org-hide-scheduled-and-waiting-next-tasks
                                       ""
                                     " (including WAITING and SCHEDULED tasks)")))
                          (org-agenda-skip-function 'd12-org--skip-projects-and-habits)
                          (org-tags-match-list-sublevels t)
                          (org-agenda-todo-ignore-scheduled d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-deadlines d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-with-date d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-sorting-strategy
                           '(todo-state-down priority-down effort-up category-keep))))
              (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil)))
              (tags-todo "-CANCELLED-READING/!"
                         ((org-agenda-overriding-header "Stuck Projects")
                          (org-agenda-skip-function 'd12-org--skip-non-stuck-projects)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-HOLD-CANCELLED-READING/!"
                         ((org-agenda-overriding-header "Projects")
                          (org-agenda-skip-function 'd12-org--skip-non-projects)
                          (org-tags-match-list-sublevels 'indented)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-READING/!"
                         ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                (if d12-org-hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                  " (including WAITING and SCHEDULED tasks)")))
                          (org-agenda-skip-function 'd12-org--skip-non-project-tasks)
                          (org-agenda-todo-ignore-scheduled d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-deadlines d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-with-date d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-READING/!"
                         ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                (if d12-org-hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                  " (including WAITING and SCHEDULED tasks)")))
                          (org-agenda-skip-function 'd12-org--skip-project-tasks)
                          (org-agenda-todo-ignore-scheduled d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-deadlines d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-with-date d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-CANCELLED+WAITING-READING|HOLD/!"
                         ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                (if d12-org-hide-scheduled-and-waiting-next-tasks
                                                                    ""
                                                                  " (including WAITING and SCHEDULED tasks)")))
                          (org-agenda-skip-function 'd12-org--skip-non-tasks)
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-todo-ignore-scheduled d12-org-hide-scheduled-and-waiting-next-tasks)
                          (org-agenda-todo-ignore-deadlines d12-org-hide-scheduled-and-waiting-next-tasks)))
              (tags "-REFILE-READING/"
                    ((org-agenda-overriding-header "Tasks to Archive")
                     (org-agenda-skip-function 'd12-org--skip-non-archivable-tasks)
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

    (setq org-habit-graph-column 60)
    (setq org-property-format "%-24s %s")
    (setq org-agenda-prefix-format
          '((agenda . " %i %-24:c%?-12t% s")
            (todo . " %i %-24:c")
            (tags . " %i %-24:c")
            (search . " %i %-24:c")))
    (setq org-agenda-todo-keyword-format "%-1s")
    (setq org-agenda-tags-column 0)

    ;; dvisvgm looks much better
    (setq org-latex-create-formula-image-program 'dvisvgm)

    :config
    ;; setup org modules
    ;; (add-to-list 'org-modules 'org-habit)
    ;; (add-to-list 'org-modules 'org-agenda)
    (require 'org-habit)
    (require 'org-agenda)
    (require 'org-capture)

    ;; Enable auto-id
    (d12-auto-id-enable)

    ;; Setup org-mode key bindings
    (d12-key-bind "C-c o r" #'org-refile org-mode-map)

    ;; fancy-yank
    (d12-key-bind "C-c y" #'fancy-yank org-capture-mode-map)

    ;; meta-return is so meta
    (d12-key-bind "<return>" 'd12-org/meta-return org-mode-map)

    ;; Setup org-agenda key bindings
    (defun d12-org/setup-agenda ()
      "Setup agenda."
      (d12-key-bind "r" #'org-agenda-refile org-agenda-mode-map))
    (add-hook 'org-agenda-mode-hook #'d12-org/setup-agenda)

    ;; automatically save all org files on certain actions
    (defadvice org-agenda-refile (after d12-org-agenda-refile-auto-save activate)
      (org-save-all-org-buffers))
    (defadvice org-agenda-todo (after d12-org-agenda-todo-auto-save activate)
      (org-save-all-org-buffers))))

(defun d12-org/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)
    (add-hook 'org-capture-mode-hook (lambda () (worf-mode -1)))))

(defun d12-org/init-ocu ()
  "Init function for `ocu' package."
  (use-package ocu
    :commands (ocu)))

(defun d12-org/init-d12-auto-id ()
  "Init function for `d12-auto-id' package."
  (use-package d12-auto-id
    :defer t))

(defun d12-org/init-org-brain ()
  (use-package org-brain
    :defer t
    :init
    (setq org-brain-path d12-path-org-notes-home)
    (d12-key-bind-personal "v" 'org-brain-visualize)
    (add-hook 'org-brain-visualize-text-hook #'d12-org/enable-latex-preview)
    :config
    (setq org-brain-visualize-sort-function 'ignore)
    (setq org-brain-visualize-one-child-per-line t)
    (setq org-id-track-globally t)
    (setq org-id-locations-file (concat d12-path-emacs-home ".org-id-locations"))
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 0)))

(defun d12-org/post-init-persp-mode ()
  (use-package persp-mode
    :init
    (spacemacs|define-custom-layout "@org"
      :binding "o"
      :body
      (find-file-existing (d12-path/get-org-file "inbox")))))

(defun d12-org/init-toc-org ()
  (use-package toc-org
    :defer t
    :init
    (progn
      (setq toc-org-max-depth 10)
      (add-hook 'org-mode-hook 'toc-org-enable))))

(defun d12-org/init-orgability ()
  (d12-path/load-project "orgability")
  (use-package orgability
    :commands (orgability-clip
               orgability-open
               orgability-add-topic
               orgability-delete-topic
               orgability-agenda-list-topics)
    :init
    (d12-key-bind-personal "C" 'orgability-clip)

    (setq orgability-agenda-topics-column 36)

    (defun d12-org/setup-agenda-orgability ()
      "Setup orgability in agenda buffer."
      (d12-key-unbind "o" org-agenda-mode-map)
      (d12-key-bind "oo" #'orgability-open org-agenda-mode-map)
      (d12-key-bind "ol" #'orgability-clip org-agenda-mode-map)
      (d12-key-bind "oa" #'orgability-add-topic org-agenda-mode-map)
      (d12-key-bind "od" #'orgability-delete-topic org-agenda-mode-map))
    (add-hook 'org-agenda-mode-hook #'d12-org/setup-agenda-orgability)

    (setq orgability-file (d12-path/get-org-file "orgability"))))

(defun d12-org/init-org-board ()
  (use-package org-board
    :commands (org-board-archive org-board-open)
    :init
    (setq org-board-wget-show-buffer nil
          org-board-default-browser 'eww)))

(defun d12-org/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (setq-default org-download-image-dir (d12-path/get-org-dir ".images/"))
    (spacemacs|use-package-add-hook org
      :post-config
      (require 'org-download))))

(defun d12-org/init-org-web-tools ()
  (use-package org-web-tools
    :commands (org-web-tools--html-title)))

;;; packages.el ends here
