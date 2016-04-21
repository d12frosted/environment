;;; packages.el --- d12frosted-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-org-packages
  '(org
    org-bullets
    org-journal))

(defun d12frosted-org/post-init-org ()
  (use-package org
    :defer t
    :init
    (d12-org/reload-agenda-files)
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ledger . t)))
    (bind-key "<f12>" 'org-agenda)
    (evil-leader/set-key-for-mode 'org-mode
      "1" 'd12-org/sort-current-level
      "!" 'd12-org/sort-upper-level
      "#" 'd12-org/insert-block-template)
    (setq org-todo-keywords
          '((sequence
             ;; The item is ready to be done at the earliest opportunity or
             ;; at the date (and maybe time) indicated in the SCHEDULED tag.
             ;; Some tasks are given a DEADLINE date which is useful for
             ;; scheduling the tasks during my daily planning.
             "TODO(t)"

             ;; I should use this tag when I start on a task, but if I clock
             ;; in to a TODO item, I don't really need this task.
             "STARTED(s)"

             ;; I did some work on this task but I am waiting for a response.
             ;; If I use this task I schedule the task into the future as a
             ;; reminder to follow up with some notes in the body of the task.
             "WAITING(w)"

             ;; Used to tag an activity that can only be done at the specified
             ;; time and date, instead of tasks that can be
             ;; completed at any time.
             "APPT(a)"

             "|"

             ;; The task is completed.
             "DONE(d)"

             ;; I decided not to do this task but have left the task on file
             ;; with this status.
             "CANCELLED(c)"

             ;; Used to identify a task that will not be activated just yet.
             ;; The reason will be included in the task notes.
             "DELAYED(l)"))

          org-agenda-window-setup 'current-window
          org-src-fontify-natively t
          org-directory d12-path/org-home
          org-agenda-inhibit-startup nil
          org-archive-location "archive/%s::"
          org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
          org-property-format "%-16s %s"
          org-agenda-prefix-format '((agenda . " %i %-24:c%?-12t% s")
                                     (timeline . "  % s")
                                     (todo . " %i %-24:c")
                                     (tags . " %i %-24:c")
                                     (search . " %i %-24:c"))
          org-agenda-day-face-function 'd12/org-agenda-day-face-holidays-function)

    (add-hook 'org-mode-hook 'd12//org-mode-setup-title)

    (d12|rename-modeline "org" org-mode "本")))

(defun d12frosted-org/post-init-org-bullets ()
  (use-package org-bullets
    :defer t
    :config
    (setq org-bullets-bullet-list '("◉" "◎" "●" "○" "•" "◦"))))

(defun d12frosted-org/init-org-journal ()
  "Initialize org-journal package"
  (use-package org-journal
    :ensure t
    :mode (".*/[0-9]*-[0-9]*-[0-9]*$" . org-journal-mode)
    :init
    (evil-leader/set-key
      ".j" 'calendar
      ".n" 'org-journal-new-entry
      ".v" 'd12-org/visit-journal-entry)
    (d12|rename-modeline "org-journal" org-journal-mode "日記")
    :config
    (global-unset-key (kbd "C-c C-j"))
    (setq org-journal-dir (concat d12-path/org-home "journal/")
          org-journal-time-format "%R\n"
          org-journal-file-format "%Y-%m-%d"
          org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
          org-journal-hide-entries-p nil)))

;;; packages.el ends here
