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
    (flexitime :location local)
    org-bullets
    ;; org-journal
    worf
    calfw
    org-gcal))

(defun d12frosted-org/post-init-org ()
  (use-package org
    :defer t
    :init
    (progn
      (d12-org/reload-files)
      (bind-key "<f12>" #'org-agenda))
    :config
    (progn
      ;; http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
      (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

      (evil-leader/set-key-for-mode
        'org-mode
        "#" 'd12-org/insert-block-template)
      (setq
       org-todo-keywords
       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
         (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))

       org-todo-state-tags-triggers
       '(("CANCELLED" ("CANCELLED" . t))
         ("WAITING" ("WAITING" . t))
         ("HOLD" ("WAITING") ("HOLD" . t))
         (done ("WAITING") ("HOLD"))
         ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
         ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
         ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

       org-hide-emphasis-markers nil
       org-agenda-window-setup 'current-window
       org-src-fontify-natively t
       org-directory d12-path/org-home
       org-default-notes-file (d12-org/get-file-path "notes")
       org-mobile-directory (concat d12-path/dropbox "Apps/d12-mobile-org")
       org-mobile-inbox-for-pull (d12-org/get-file-path "inbox")
       org-agenda-inhibit-startup nil
       org-archive-location "archive/%s_archive::"
       org-time-clocksum-format
       '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
       org-property-format "%-16s %s"
       org-agenda-prefix-format '((agenda . " %i %-24:c%?-12t% s")
                                  (timeline . "  % s")
                                  (todo . " %i %-24:c")
                                  (tags . " %i %-24:c")
                                  (search . " %i %-24:c"))
       org-agenda-sorting-strategy '((agenda habit-up time-up scheduled-down deadline-down todo-state-up category-keep priority-down)
                                     (todo todo-state-up priority-down category-keep)
                                     (tags todo-state-up priority-down category-keep)
                                     (search todo-state-up priority-down category-keep))
       org-agenda-day-face-function 'd12-org/agenda-day-face-holidays-function

       org-capture-templates
       `(("t" "todo" plain (file ,(d12-org/get-file-path "inbox"))
          "* TODO %^{Task} %?")
         ("j" "Journal entry" plain
          (file+datetree+prompt ,(d12-org/get-file-path "journal"))
          "%i\n%?\n")))

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

      ;; setup appt
      ;; TODO: defer it's activation
      (require 'appt)
      (appt-activate t)

      (setq appt-display-mode-line nil
            ;; Show notification 5 minutes before event
            appt-message-warning-time 10
            ;; Disable multiple reminders
            appt-display-interval 5

            ;; Display appointments as a window manager notification
            appt-disp-window-function 'd12-org/appt-display
            appt-delete-window-function (lambda () t))

      (defun d12-org/agenda-to-appt ()
        (interactive)
        (setq appt-time-msg-list nil)
        (org-agenda-to-appt))

      (defun d12-org//appt-alert (min-to-app msg)
        (alert (format "In %s minute(s)" min-to-app)
               :title msg))

      (defun d12-org/appt-display (min-to-app new-time msg)
        (if (atom min-to-app)
            (d12-org//appt-alert min-to-app msg)
          (dolist (i (number-sequence 0 (1- (length min-to-app))))
            (d12-org//appt-alert (nth i min-to-app) (nth i msg)))))

      (when d12-org/enable-notifications
        ;; Update alarms when...
        ;; (1) ... Starting Emacs
        (d12-org/agenda-to-appt)
        ;; (2) ... Everyday at 12:05am (useful in case you keep Emacs always on)
        (run-at-time "12:05am" (* 24 3600) 'd12-org/agenda-to-appt)
        ;; (3) when agenda is displayed
        (add-hook 'org-finalize-agenda-hook 'd12-org/agenda-to-appt 'append)

        ;; Run `org-self-insert-command' only if `d12-org/insert-org-entity-maybe'
        ;; returns nil.
        ;;
        ;; http://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification/16746#16746
        (advice-add 'org-self-insert-command :before-until #'d12-org/insert-org-entity-maybe))

      ;; that's all
      )))

(defun d12frosted-org/init-flexitime ()
  (use-package flexitime
    :after org
    :init
    (setq flexitime-weekday-duration 540)))

(defun d12frosted-org/post-init-org-bullets ()
  (use-package org-bullets
    :defer t
    :config
    (setq org-bullets-bullet-list '("◉" "◎" "●" "○" "•" "◦"))))

(defun d12frosted-org/init-org-journal ()
  "Initialize org-journal package"
  (use-package org-journal
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

(defun d12frosted-org/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)))

(defun d12frosted-org/init-calfw ()
  (use-package calfw
    :init
    (require 'calfw-org)))

(defun d12frosted-org/init-org-gcal ()
  (use-package org-gcal
    :init
    (setq
     org-gcal-client-id d12-gcal-client-id
     org-gcal-client-secret d12-gcal-client-secret
     org-gcal-file-alist (mapcar
                          (lambda (el)
                            (cons (car el)
                                  (concat d12-path/org-home (cdr el))))
                          d12-gcal-calendar-alist))
    :config
    (defun org-gcal--notify (title mes)
      (message "org-gcal::%s - %s" title mes))))

;;; packages.el ends here
