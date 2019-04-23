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

(defvar +org-refile-ignore-tags '("JOURNAL" "REFILE")
  "List of tags to ignore during refile.")

;;
;; Org modules

;; Setup list of Org modules that should always be loaded together
;; with Org.
(defvar org-modules
  '(org-info
    org-habit))

;; Sub-modules
(load! "+agenda")
(load! "+capture")
(load! "+auto-id")
(load! "+auto-save")
(load! "+modeline")

;; Load other Org modules lazily.
(nucleus-load-packages-incrementally
 '(org-agenda
   org-capture
   org-id
   org-attach
   org-edna))

;;
;; Bootstrap

(add-hook! 'org-load-hook
  #'(+org|setup-ui
     +org|setup-todo
     +org|setup-tags
     +org|setup-clock
     +org|setup-id
     +org|setup-refile
     +org|setup-attach))

(add-hook! 'org-mode-hook
  #'(org-indent-mode
     org-edna-load
     auto-fill-mode
     places-mode-maybe-enable
     pretty-props-mode-maybe-enable
     cha-mode-maybe-enable))

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
	      org-startup-indented t
	      org-outline-path-complete-in-steps nil

	      ;; better formatting for properties
	      org-property-format "%-24s %s"

        ;; LaTeX preview
        org-format-latex-options
        (list :foreground default
              :background default
              :scale 1.5
		          :html-foreground "Black"
              :html-background "Transparent"
		          :html-scale 1.0
              :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

(defun +org|setup-todo ()
  "Setup todo states."
  (setq org-todo-keywords
	      '((sequence "TODO(t)" "|" "DONE(d!)")
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
          (done ("WAITING") ("HOLD") ("FOCUS"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

	      ;; use drawer for state changes
	      org-log-into-drawer t))

(defun +org|setup-tags ()
  "Setup tags-related parts of Org."
  (setq org-tag-persistent-alist '(("FOCUS" . ?f)
                                   ("PROJECT" . ?p)))
  (add-to-list 'org-tags-exclude-from-inheritance "PROJECT"))

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

(defun +org|setup-id ()
  "Setup `org-id'."
  (setq org-id-track-globally t
	      org-id-locations-file (concat nucleus-cache-dir "org-id-locations.el")))

(defun +org|setup-refile ()
  "Setup refile functionality."
  (setq org-refile-targets '((nil :maxlevel . 4)
			                       (org-agenda-files :maxlevel . 4))
	      org-refile-use-outline-path t
	      org-refile-allow-creating-parent-nodes nil
	      org-refile-target-verify-function '+org-refile--verify-refile-target)

  (defun +org-refile--verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (let ((tags-at (org-get-tags-at)))
      (and
       ;; doesn't have done keyword
       (not (member (nth 2 (org-heading-components)) org-done-keywords))

       ;; doesn't have blacklisted tag
       (or (null tags-at)
           (member-if-not
            (lambda (x)
              (member (if (listp x) (car x) x) +org-refile-ignore-tags))
            tags-at))))))

(defun +org|setup-attach ()
  "Setup attach functionality."
  (setq org-attach-directory ".data"
        org-attach-auto-tag nil
        org-attach-file-list-property nil
        org-attach-store-link-p 'attached))
;;
;; Packages

(def-package! org-brain
  :defer t
  :init
  (setq org-brain-path (concat org-directory "notes/")
	      org-brain-visualize-sort-function #'org-brain-title<
	      org-brain-visualize-one-child-per-line t
	      org-brain-visualize-default-choices 'all
	      org-brain-title-max-length 24)
  (add-hook 'org-brain-visualize-text-hook #'org-toggle-latex-fragment)
  :config
  (map! :map org-brain-visualize-mode-map
        :leader
        :desc "Button" "jb" #'+ace-link-brain-visualize))

(def-package! orgability
  :defer t
  :init
  (setq
   orgability-file (concat org-directory "orgability.org")
   orgability-agenda-topics-column 36))

(def-package! org-board
  :defer t
  :init
  (setq org-board-wget-show-buffer nil
        org-board-default-browser 'eww))

(def-package! org-download
  :defer t
  :init
  (setq-default org-download-method 'attach)
  (add-hook 'org-mode-hook #'org-download-enable))
