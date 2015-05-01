;;; packages.el --- d12frosted-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-org-packages
  '(org)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-org-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted-org/init-org ()
  "Initialize org package."
  (use-package org
    :defer 1
    :init
    :config

    (setq-local d12/org-ignored-dirs
                (-flatten
                 (-non-nil
                  (-map (lambda (dir)
                          (d12frosted/org-dir-and-subdirs dir))
                        d12frosted/org-agenda-ignore-dirs))))

    (setq-local d12/org-agenda-dirs
          (-difference (d12frosted/org-dir-and-subdirs "") d12/org-ignored-dirs))

    (setq-local d12/org-agenda-files
          (-flatten (-map (lambda (dir)
                            (d12frosted/org-files-in-folder dir))
                          d12/org-agenda-dirs)))

    (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
      (let ((rlt ad-return-value)
            (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
            (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
            old-flag
            b e)
        (when ad-return-value
          (save-excursion
            (setq old-flag case-fold-search)
            (setq case-fold-search t)
            (setq b (re-search-backward begin-regexp nil t))
            (if b (setq e (re-search-forward end-regexp nil t)))
            (setq case-fold-search old-flag))
          (if (and b e (< (point) e)) (setq rlt nil)))
        (setq ad-return-value rlt)))

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
          org-directory d12frosted/org-home-path
          org-agenda-files d12/org-agenda-files
          org-agenda-inhibit-startup nil
          org-mobile-inbox-for-pull (s-concat d12frosted/org-home-path "mobile.org")
          org-mobile-force-id-on-agenda-items nil
          org-mobile-directory "~/Dropbox/Apps/MobileOrg/")

    (evil-leader/set-key-for-mode 'org-mode
      "m C-s" 'd12frosted/org-sort-current-level
      "m C-S" 'd12frosted/org-sort-upper-level
      "m#" 'd12frosted/org-insert-block-template
      "m C-d" 'd12frosted/org-insert-date
      "m C-t" 'd12frosted/org-insert-time)

    (spacemacs/declare-prefix "oj" "org/journal")

    (evil-leader/set-key "ojl" 'org-store-link)
    (evil-leader/set-key "oit" 'd12frosted/org-insert-time)
    (evil-leader/set-key "oid" 'd12frosted/org-insert-date)

    (add-hook 'org-mode-hook 'd12frosted/org-auto-insert-template)))
