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
(require 'init-env)
(require 'init-package)
(require 'init-keybindings)
(require 'init-navigation)
(require 'cl-lib)
(require '+company)

(defvar +capture-inbox-file (format "inbox-%s.org" +sys-name)
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
  :straight org-plus-contrib
  :commands (org-map-entries)
  :hook ((org-mode . auto-fill-mode)
         (org-mode . places-mode-maybe-enable)
         (org-mode . pretty-props-mode-maybe-enable)
         (org-mode . cha-mode-maybe-enable)
         (org-mode . wine-mode-maybe-enable)
         (org-mode . cigars-mode-maybe-enable)
         (org-clock-out . +org/remove-empty-drawer))
  :init
  ;; Setup list of Org modules that should always be loaded together
  ;; with Org.
  (setq org-modules
        '(org-habit
          org-agenda
          org-archive
          org-capture
          org-id
          org-attach
          org-edna
          ob-emacs-lisp
          ob-dot
          ob-plantuml))
  (setq org-directory (concat +path-home-dir "Dropbox/vulpea/"))
  :config
  (require '+org-auto-id)
  (require '+org-agenda)
  (require '+org-cha)
  (require '+org-wine)
  (require '+org-cigars)
  (require '+org-dependent)

  ;; open files in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (setq org-indirect-buffer-display 'current-window)

  ;; open directory links in `dired'
  (add-to-list 'org-file-apps '(directory . emacs))

  (setq
   org-adapt-indentation nil
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-image-actual-width '(512)
   org-imenu-depth 2
   org-pretty-entities nil
   org-startup-folded t
   org-startup-indented t

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
   org-tags-exclude-from-inheritance '("PROJECT")))

(use-package org-clock
  :defer t
  :straight org-plus-contrib
  :init
  (setq
   ;; remove clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t))

(use-package org-refile
  :defer t
  :straight org-plus-contrib
  :init
  (setq org-outline-path-complete-in-steps nil
        org-refile-targets '((nil :maxlevel . 4)
                             (org-agenda-files :maxlevel . 4))
        org-refile-use-outline-path t
        org-refile-allow-creating-parent-nodes nil
        org-refile-target-verify-function '+org-refile--verify-refile-target))

(use-package org-indent
  :defer t
  :straight org-plus-contrib
  :diminish org-indent-mode
  :hook ((org-mode . org-indent-mode)))

(use-package org-id
  :defer t
  :straight org-plus-contrib
  :hook ((before-save . +org-auto-id-add-to-headlines-in-file)
	       (org-capture-prepare-finalize . +org-auto-id-dwim))
  :init
  (setq org-id-uuid-program "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
  :config
  (setq org-id-track-globally t
        org-id-extra-files (list (expand-file-name ".archive/archive" org-directory)
                                 (expand-file-name ".archive/archive.org" org-directory))
	      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-id-locations-file (expand-file-name ".orgids" org-directory)))

(use-package org-capture
  :defer t
  :straight org-plus-contrib
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
  :straight org-plus-contrib
  :defines (org-attach-file-list-property)
  :config
  (setq org-attach-id-dir ".data/"
        org-attach-auto-tag nil
        org-attach-file-list-property nil
        org-attach-store-link-p 'attached))

(use-package ox-latex
  :defer t
  :straight org-plus-contrib
  :commands (org-latex-preview)
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
  :straight org-plus-contrib
  :config
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))

(use-package ob-plantuml
  :defer t
  :straight org-plus-contrib
  :defines (org-plantuml-jar-path))

(use-package org-archive
  :defer t
  :straight org-plus-contrib
  :config
  (setq
   org-archive-location (concat org-directory ".archive/archive_%s" "::" "datetree/*")
   org-archive-save-context-info '(time file ltags itags todo category olpath)))

(use-package org-agenda
  :defer t
  :straight org-plus-contrib
  :commands (org-agenda-refile
             org-agenda-todo
             org-agenda-clock-in
             org-agenda-clock-out)
  :init
  ;; some aggressive saving
  (advice-add #'org-agenda-refile :after #'+org-save-all)
  (advice-add #'org-agenda-todo :after #'+org-save-all)
  (advice-add #'org-agenda-clock-in :after #'+org-save-all)
  (advice-add #'org-agenda-clock-out :after #'+org-save-all)
  :config
  (eval-when-compile
    (require '+org-agenda))
  (setq
   org-agenda-files (list org-directory)
   org-agenda-text-search-extra-files (list (expand-file-name ".archive/archive" org-directory)
                                            (expand-file-name ".archive/archive.org" org-directory))
   ;; also show state change in log mode
   org-agenda-log-mode-items '(closed clock state)

   ;; more structured view
   org-agenda-prefix-format
   '((agenda . " %i %-24:c%?-12t %12s")
     (todo . " %i %-24:c")
     (tags . " %i %-24:c")
     (search . " %i %-24:c"))
   org-agenda-todo-keyword-format "%-1s"
   org-agenda-tags-column 0

   ;; show agenda in current window
   org-agenda-window-setup 'current-window

   org-agenda-custom-commands
   `((" " "Agenda"
      (,+agenda--refile
       ,+agenda--today
       ,+agenda--focus
       ,+agenda--stuck-projects
       ,+agenda--projects
       ,+agenda--waiting)
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
  :hook ((org-mode . org-edna-mode)))

(use-package org-brain
  :defer t
  :defines (org-brain-targets-match)
  :commands (org-brain-title<
             org-brain-path-entry-name
             org-brain-entry-name
             org-brain-entry-from-id
             org-brain--name-and-id-at-point)
  :init
  (setq org-brain-path org-directory
        org-brain-scan-directories-recursively nil
        org-brain-targets-match "NOTE"
	      org-brain-visualize-sort-function #'org-brain-title<
        org-brain-child-linebreak-sexp 0
	      org-brain-visualize-default-choices 'all
	      org-brain-title-max-length 36)
  (add-hook 'org-brain-visualize-text-hook #'org-latex-preview)
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

(use-package org-drawer-list
  :defer t
  :straight (org-drawer-list
             :type git
             :host github
             :repo "d12frosted/org-drawer-list"))

(use-package orgability
  :defer t
  :straight (orgability
             :type git
             :host github
             :repo "d12frosted/orgability")
  :defines (orgability-file
            orgability-agenda-topics-column)
  :init
  (setq
   orgability-file (expand-file-name "orgability.org" org-directory)
   orgability-agenda-topics-column 36))

(use-package org-board
  :defer t
  :init
  (setq org-board-wget-show-buffer nil
        org-board-default-browser 'eww))

(use-package org-download
  :defer t
  :hook ((org-mode . org-download-enable))
  :init
  (setq-default org-download-method 'attach))

(use-package toc-org
  :defer t
  :after org
  :init
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package +org-notes
  :defer t
  :straight (:type built-in)
  :commands (+org-notes-list
             +org-notes-find
             +org-notes-setup-buffer)
  :init
  (setq +org-notes-directory (concat org-directory "notes/"))
  (add-to-list 'window-buffer-change-functions
               #'+org-notes-setup-buffer))

(use-package org-roam
  :defer t
  :straight (org-roam
             :type git
             :host github
             :repo "org-roam/org-roam")
  :commands (org-roam-dailies-today
             org-roam-dailies-yesterday
             org-roam-dailies-tomorrow
             org-roam-find-file
             org-roam-insert
             org-roam-db--clear
             org-roam-db-build-cache
             org-roam--get-title-or-slug)
  :init
  (setq org-roam-directory +org-notes-directory
        org-roam-db-location (expand-file-name "org-roam.db" +path-cache-dir)
        org-roam-graph-viewer
        (when +sys-mac-p "open")
        org-roam-graph-executable (executable-find "neato")
        org-roam-graph-extra-config '(("overlap" . "false"))
        org-roam-completion-system 'ivy
        org-roam-graph-exclude-matcher '("literature_notes"
                                         "permanent_notes"
                                         "inbox"
                                         "unfinished"
                                         "people"
                                         "book"
                                         "project"

                                         ;; very specific notes
                                         "20200406121527"
                                         "20200406121532"
                                         "20200401163827"
                                         "20200401163611"
                                         "20200401163758"
                                         "20200407181600"
                                         "20200430184542"))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+OPTIONS: auto-id:t\n#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "daily" plain #'org-roam-capture--get-point
           ""
           :immediate-finish t
           :file-name "%<%Y-%m-%d>"
           :head "#+OPTIONS: auto-id:t\n#+TITLE: %<%A, %d %B %Y>\n#+TIME-STAMP: <>\n\n")))
  (require 'org-protocol)
  (require 'org-roam-protocol))

(use-package company-org-roam
  :straight (:host github :repo "org-roam/company-org-roam")
  :init
  (set-company-backend! 'org-mode
    '(company-org-roam company-yasnippet company-dabbrev)))

(use-package org-roam-server
  :straight (org-roam-server
             :host github
             :repo "org-roam/org-roam-server"
             :files (:defaults ("assets/" . "assets/")))
  :load-path (lambda () (concat straight-base-dir "straight/repos/org-roam-server/"))
  :defer t)

(use-package time-stamp
  :straight (:type built-in)
  :init
  (add-hook 'write-file-functions 'time-stamp))

(use-package org-journal
  :defer
  :init
  (setq
   org-journal-find-file #'find-file
   org-journal-hide-entries-p nil
   org-journal-time-prefix "* "
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-dir +org-notes-directory
   org-journal-cache-file (expand-file-name "org-journal.cache" +path-cache-dir)

   ;; strictly speaking, these are not used, because I let `org-roam' create new
   ;; file via `+org-notes-new-journal-entry'.
   org-journal-date-prefix "#+TITLE: "
   org-journal-file-header "#+TIME-STAMP: <>"
   org-journal-date-format "%A, %d %B %Y")
  (set-company-backend! 'org-journal-mode
    '(company-org-roam company-yasnippet company-dabbrev)))

;; remove after
;; https://github.com/jrblevin/deft/issues/77
(with-no-warnings
  (require 'cl))

(use-package deft
  :defer t
  :init
  (setq
   deft-recursive t
   deft-use-filter-string-for-filename t
   deft-default-extension "org"
   deft-directory +org-notes-directory)
  :config/el-patch
  (defun deft-parse-title (file _)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
                       (deft-base-filename file)
                     (let ((begin (string-match "^.+$" contents)))
                       (if begin
                           (funcall deft-parse-title-function
                                    (substring contents begin (match-end 0))))))
                   (org-roam--get-title-or-slug file))))

;;; Functions

(declare-function org-remove-empty-drawer-at "org")
(declare-function org-get-tags "org")
(declare-function org-heading-components "org")
(declare-function org-save-all-org-buffers "org")

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
