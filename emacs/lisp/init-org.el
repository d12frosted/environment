;;; init-org.el --- org-mode init file -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-path)
(require 'cl-lib)

(require '+org-cha)
(require '+org-cigars)
(require '+org-dependent)

;; Setup list of Org modules that should always be loaded together
;; with Org.
(defvar org-modules
  '(org-info
    org-habit
    org-agenda
    org-archive
    org-capture
    org-id
    org-attach
    org-edna
    ob-emacs-lisp
    ob-dot
    ob-plantuml))

(defvar +capture-inbox-file "inbox.org"
  "The path to the inbox file.

It is relative to `org-directory', unless it is absolute.")

(defvar +capture-journal-file "journal.org"
  "The path to the journal file.

It is relative to `org-directory', unless it is absolute.")

(defvar +org-refile-ignore-tags '("JOURNAL" "REFILE")
  "List of tags to ignore during refile.")

(defvar org-export-backends
  '(beamer
    latex
    html))

(use-package org
  :defer t
  :ensure org-plus-contrib
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode)
         (org-mode . places-mode-maybe-enable)
         (org-mode . pretty-props-mode-maybe-enable)
         (org-mode . cha-mode-maybe-enable)
         (org-mode . cigars-mode-maybe-enable)
         (org-clock-out . +org/remove-empty-drawer))
  :general
  (+leader-def
    "oa" '(+agenda/main :which-key "Org fast agenda")
    "ow" '(+agenda/wix :which-key "Org Wix fast agenda")
    "or" '(+orgability/list :which-key "Reading list")
    "oA" '(org-agenda :which-key "Org agenda"))
  :init
  (setq org-directory (concat +path-home-dir "Dropbox/vulpea/"))
  ;; some aggressive saving
  (advice-add #'org-agenda-refile :after #'+org-save-all)
  (advice-add #'org-agenda-todo :after #'+org-save-all)
  (advice-add #'org-agenda-clock-in :after #'+org-save-all)
  (advice-add #'org-agenda-clock-out :after #'+org-save-all)
  :config
  (require '+org-auto-id)
  (require '+org-agenda)

  (setq
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-image-actual-width '(512)
   org-pretty-entities nil
   org-adapt-indentation nil
   org-startup-folded t
   org-startup-indented t
   org-outline-path-complete-in-steps nil

   ;; do not allow invisible edits (...)
   org-catch-invisible-edits 'error

   ;; formatting for properties
   org-property-format "%-24s %s"

   ;; LaTeX preview
   org-format-latex-options
   (list :foreground 'default
         :background 'default
         :scale 1.5
         :html-foreground "Black"
         :html-background "Transparent"
         :html-scale 1.0
         :matchers '("begin" "$1" "$" "$$" "\\(" "\\["))

   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)"
               "HOLD(h@/!)"
               "|"
               "CANCELLED(c@/!)"
               "MEETING"))

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
   org-log-into-drawer t

   ;; tags
   org-tag-persistent-alist '(("FOCUS" . ?f)
                              ("PROJECT" . ?p))
   org-tags-exclude-from-inheritance '("PROJECT")

   ;; remove clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t

   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))
   org-refile-use-outline-path t
   org-refile-allow-creating-parent-nodes nil
   org-refile-target-verify-function '+org-refile--verify-refile-target))

(use-package org-id
  :defer t
  :ensure org-plus-contrib
  :hook ((before-save . +org-auto-id-add-to-headlines-in-file)
	 (org-capture-prepare-finalize . +org-auto-id-dwim))
  :config
  (setq org-id-track-globally t
	org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
	org-id-locations-file (concat +path-cache-dir "org-id-locations.el")))

(use-package org-capture
  :defer t
  :ensure org-plus-contrib
  :general
  (+leader-def
    "cj" '(+org/capture-journal :which-key "Capture journal")
    "cm" '(+org/capture-meeting :which-key "Capture meeting")
    "cn" '(+org/capture-note :which-key "Capture note")
    "cx" '(+org/capture-task :which-key "Capture task")
    "cX" '(org-capture :which-key "Org capture")
    "cl" '(org-store-link :which-key "Org store link"))
  :config
  (require '+org-capture)
  (dolist (var '(+capture-inbox-file
                 +capture-journal-file))
    (set var (expand-file-name (symbol-value var) org-directory)))
  (unless org-default-notes-file
    (setq org-default-notes-file +capture-inbox-file))
  (setq org-capture-templates
	'(("t" "todo" plain (file +capture-inbox-file)
	   "* TODO %?\n%U\n" :clock-in t :clock-resume t)

	  ("j" "Journal" entry (file+olp+datetree +capture-journal-file)
	   "* %?\n%U\n" :clock-in t :clock-resume t :time-prompt t)
  
	  ("n" "note" entry (file +capture-inbox-file)
	   "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

	  ("m" "Meeting" entry (file +capture-inbox-file)
	   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))

(use-package org-attach
  :defer t
  :ensure org-plus-contrib
  :config
  (setq org-attach-directory ".data"
        org-attach-auto-tag nil
        org-attach-file-list-property nil
        org-attach-store-link-p 'attached))

(use-package ox-latex
  :defer t
  :ensure org-plus-contrib
  :config
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (setq org-latex-listings 'minted)
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(use-package ox-beamer
  :defer t
  :ensure org-plus-contrib
  :config
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))

(use-package org-archive
  :defer t
  :ensure org-plus-contrib
  :config
  (setq
   org-archive-location (concat org-directory ".archive/archive_%s" "::" "datetree/*")
   org-archive-save-context-info '(time file ltags itags todo category olpath)))

(use-package org-agenda
  :defer t
  :ensure org-plus-contrib
  :config
  (setq
   org-agenda-files (list org-directory)
   ;; also show state change in log mode
   org-agenda-log-mode-items '(closed clock state)

   ;; more structured view
   org-agenda-prefix-format
   '((agenda . " %i %-24:c%?-12t% s")
     (todo . " %i %-24:c")
     (tags . " %i %-24:c")
     (search . " %i %-24:c"))
   org-agenda-todo-keyword-format "%-1s"
   org-agenda-tags-column 0

   ;; show agenda in current window
   org-agenda-window-setup 'current-window

   org-agenda-custom-commands
   `(
     (" " "Agenda"
      (
       (tags "REFILE"
	           ((org-agenda-overriding-header "To refile")
	            (org-tags-match-list-sublevels nil)))
       (agenda "" ((org-agenda-span 'day)
		               (org-agenda-skip-deadline-prewarning-if-scheduled t)
		               (org-agenda-sorting-strategy
		                '(habit-down time-up category-keep todo-state-down priority-down))))
       (tags-todo "PROJECT-CANCELLED-HOLD/!"
		              ((org-agenda-overriding-header "Stuck Projects")
		               (org-agenda-skip-function '+agenda--skip-non-stuck-projects)
		               (org-agenda-sorting-strategy
		                '(todo-state-down priority-down effort-up category-keep))))
       (tags-todo "PROJECT-HOLD"
		              ((org-agenda-overriding-header (concat "Projects"))
		               (org-tags-match-list-sublevels t)
		               (org-agenda-skip-function '+agenda--skip-non-projects)
		               (org-agenda-tags-todo-honor-ignore-options t)
		               (org-agenda-sorting-strategy
		                '(todo-state-down priority-down effort-up category-keep))))
       (tags-todo "FOCUS"
		              ((org-agenda-overriding-header
		                (concat "To focus on"
			                      (if +agenda-hide-scheduled-and-waiting-next-tasks
				                        ""
			                        " (including WAITING and SCHEDULED tasks)")))
		               (org-agenda-skip-function '+agenda--skip-habits)
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
       ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
       ;;            ((org-agenda-overriding-header
       ;;              (concat "Project Subtasks"
       ;;                      (if +agenda-hide-scheduled-and-waiting-next-tasks
       ;;                          ""
       ;;                        " (including WAITING and SCHEDULED tasks)")))
       ;;             (org-agenda-skip-function '+agenda--skip-non-project-tasks)
       ;;             (org-agenda-todo-ignore-scheduled +agenda-hide-scheduled-and-waiting-next-tasks)
       ;;             (org-agenda-todo-ignore-deadlines +agenda-hide-scheduled-and-waiting-next-tasks)
       ;;             (org-agenda-todo-ignore-with-date +agenda-hide-scheduled-and-waiting-next-tasks)
       ;;             (org-agenda-sorting-strategy
       ;;              '(todo-state-down priority-down effort-up category-keep))))
       (tags-todo "-CANCELLED+WAITING-READING-FOCUS|+HOLD/!"
		              ((org-agenda-overriding-header
		                (concat "Waiting and Postponed Tasks"
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
	      '((tags . "%(orgability-agenda-list-topics)")))
       (org-agenda-sorting-strategy
	      '(todo-state-down effort-up category-keep))
       (org-agenda-buffer-name +agenda-reading-list-buffer-name)))

     ("h" "Habits" tags-todo "STYLE=\"habit\""
      ((org-agenda-overriding-header "Habits")
       (org-agenda-sorting-strategy
	      '(todo-state-down effort-up category-keep))
       (org-agenda-buffer-name +agenda-habits-buffer-name)))

     )))

(use-package org-edna
  :defer t
  :ensure org-plus-contrib
  :hook ((org-mode . org-edna-load)))

(use-package org-brain
  :defer t
  :general
  (+leader-def
    "ob" '(org-brain-visualize :which-key "Brain node"))
  (+leader-def
    :keymaps 'org-brain-visualize-mode-map
    "jb" '(+ace-link-brain-visualize :which-key "Brain entry"))
  :init
  (setq org-brain-path org-directory
        org-brain-scan-directories-recursively nil
        org-brain-targets-match "NOTE"
	      org-brain-visualize-sort-function #'org-brain-title<
	      org-brain-visualize-one-child-per-line t
	      org-brain-visualize-default-choices 'all
	      org-brain-title-max-length 24)
  (add-hook 'org-brain-visualize-text-hook #'org-toggle-latex-fragment)
  :config
  (defun +ace-link-brain-visualize ()
    "Open a visible link in an `org-brain-visualize-mode' buffer."
    (interactive)
    (ace-link-help))
  (defun org-brain--file-targets (file)
    "Return alist of (name . entry-id) for all entries (including the file) in FILE."
    (let* ((file-relative (org-brain-path-entry-name file))
           (file-entry-name (org-brain-entry-name file-relative)))
      (append (list (cons file-entry-name file-relative))
              (with-temp-buffer
                (insert-file-contents file)
                (delay-mode-hooks
                  (org-mode)
                  (mapcar (lambda (entry)
                            (cons (concat file-entry-name "::" (car entry))
                                  (cadr entry)))
                          (remove nil (org-map-entries
                                       #'org-brain--name-and-id-at-point
                                       org-brain-targets-match)))))))))

(use-package orgability
  :defer t
  :general
  (+leader-def
    "co" '(orgability-clip :which-key "Orgability clip"))
  :init
  (setq
   orgability-file (concat org-directory "orgability.org")
   orgability-agenda-topics-column 36))

(use-package org-board
  :defer t
  :init
  (setq org-board-wget-show-buffer nil
        org-board-default-browser 'eww))

(use-package org-download
  :defer t
  :hook ((org-mode-hook . org-download-enable))
  :init
  (setq-default org-download-method 'attach))

;;; Functions

(declare-function org-remove-empty-drawer-at "org")
(declare-function org-get-tags "org")
(declare-function org-heading-components "org")
(declare-function org-save-all-org-buffers "org")

(defun +org/remove-empty-drawer ()
  "Remove empty drawer at point."
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(defun +org-refile--verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (let ((tags-at (org-get-tags)))
    (and
     ;; doesn't have done keyword
     (not (member (nth 2 (org-heading-components)) org-done-keywords))

     ;; doesn't have blacklisted tag
     (or (null tags-at)
         (cl-member-if-not
          (lambda (x)
            (member (if (listp x) (car x) x) +org-refile-ignore-tags))
          tags-at)))))

;;;###autoload
(defun +org-save-all (&rest args)
  "Save all Org buffers.

ARGS are ignored."
  (let ((_ args)))
  (org-save-all-org-buffers))

(provide 'init-org)
;;; init-org.el ends here
