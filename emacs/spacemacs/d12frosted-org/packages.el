;;; packages.el --- d12frosted-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

 ;;; Code:

(defconst d12frosted-org-packages
  '(org
    ;; (flexitime :location local)
    (d12-gtd :location local)
    (d12-capture-link :location local)
    (org-generate :location local)
    (org-query :location
               (recipe
                :fetcher github
                :repo "remyhonig/org-query"
                :files ("org-query.el"
                        "org-query-gtd.el")))
    org-bullets
    (org-protocol-capture-html :location
                               (recipe
                                :fetcher github
                                :repo "alphapapa/org-protocol-capture-html"))
    worf))

(defun d12frosted-org/post-init-org ()
  (use-package org
    :defer t
    :init
    (progn
      (bind-key "<f12>" #'org-agenda)
      (bind-key "C-<f12>" 'd12-capture-link))
    :config
    (progn
      ;; setup d12-gtd
      (require 'd12-gtd)

      ;; generate unique headline ids
      (require 'org-id)
      (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

      ;; use return to follow/activate link
      (setq org-return-follows-link t)

      ;; http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
      (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

      (setq org-agenda-day-face-function #'d12-org/agenda-day-face-holidays-function)

      (setq
       org-startup-indented t
       org-hide-emphasis-markers nil
       org-src-fontify-natively t)

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
        (advice-add 'org-self-insert-command :before-until #'d12-org/insert-entity-maybe))

      ;; that's all
      )))

(defun d12frosted-org/init-d12-gtd ()
  (use-package d12-gtd
    :defer t))

(defun d12frosted-org/init-d12-capture-link ()
  (use-package d12-capture-link
    :commands (d12-capture-link)))

(defun d12frosted-org/init-org-generate ()
  (use-package org-generate))

(defun d12frosted-org/init-org-query ()
  (use-package org-query
    :defer t))

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

(defun d12frosted-org/init-org-protocol-capture-html ()
  (use-package org-protocol-capture-html
    :defer t))

(defun d12frosted-org/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)
    (add-hook 'org-capture-mode-hook (lambda () (worf-mode -1)))))

;;; packages.el ends here
