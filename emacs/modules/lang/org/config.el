;;; lang/org/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 21 Nov 2018
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

;;
;; Variables

;; Setup location of Org files. Should be set before Org loads.
(defvar org-directory (concat nucleus-home-dir "Dropbox/vulpea/"))

;;
;; Org modules

;; Setup list of Org modules that should always be loaded together
;; with Org.
(defvar org-modules
  '(org-info
    org-habit))

;; Sub-modules
(load! "+agenda")

;; Load other Org modules lazily.
(nucleus-load-packages-incrementally
 '(org-agenda
   org-capture))

;;
;; Bootstrap

(add-hook! 'org-load-hook
  #'(+org|setup-ui
     +org|setup-todo
     +org|setup-clock))

;;
;; `org-load-hook'

(defun +org|setup-ui ()
  "Setup UI of `org-mode'."
  (setq org-hidden-keywords nil
	org-hide-emphasis-markers nil
	org-hide-leading-stars nil
	org-image-actual-width '(512)
	org-pretty-entities nil
	org-adapt-indentation nil
	org-startup-folded t
	org-startup-indented t))

(defun +org|setup-todo ()
  "Setup todo states."
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))

	;; use fast todo selection
	org-use-fast-todo-selection t

	;; allo to fast fix todo state without triggering anything
	org-treat-S-cursor-todo-selection-as-state-change nil

	;; setup state triggers
	org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

	;; use drawer for state changes
	org-log-into-drawer t))

(defun +org|setup-clock ()
  "Setup clock-related parts of Org."
  ;; remove clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; remove empty drawers on clock out
  (defun +org/remove-empty-drawer ()
    "Remove empty drawer at point."
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at (point))))
  (add-hook! :append 'org-clock-out-hook #'+org/remove-empty-drawer))
