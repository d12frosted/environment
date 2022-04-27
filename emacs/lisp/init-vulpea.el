;;; init-vulpea.el --- Note taking and project management  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 08 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module configures all things for note taking and project
;; management. The stars are `org' and `org-roam'.
;;
;;; Code:

(require 'config-path)
(require 'init-elpa)
(require 'init-env)
(require 'init-selection)
(require 'config-vulpea)



(use-package vulpea
  :straight (vulpea
             :type git
             :host github
             :repo "d12frosted/vulpea")
  :defer t
  :general
  (leader-def
    "n" '(nil :which-key "vulpea...")
    "nd" '(nil :which-key "by date...")
    "ndd" '(vulpea-dailies-date :which-key "arbitrary date")
    "ndt" '(vulpea-dailies-today :which-key "today")
    "ndn" '(vulpea-dailies-next :which-key "next")
    "ndp" '(vulpea-dailies-prev :which-key "previous")
    "nf" '(vulpea-find :which-key "find")
    "nF" '(vulpea-find-backlink :which-key "find backlink")
    "ni" '(vulpea-insert :which-key "insert")
    "nt" '(vulpea-tags-add :which-key "tag")
    "nT" '(vulpea-tags-delete :which-key "untag")
    "na" '(vulpea-alias-add :which-key "alias")
    "nA" '(vulpea-alias-delete :which-key "unalias")
    "ol" '(litnotes :which-key "litnotes"))
  :hook ((before-save . vulpea-pre-save-hook)
         (org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :init
  (add-to-list 'window-buffer-change-functions
               #'vulpea-setup-buffer)
  (add-hook 'vulpea-insert-handle-functions
            #'vulpea-insert-handle)
  (setq-default
   vulpea-find-default-filter
   (lambda (note)
     (= (vulpea-note-level note) 0))
   vulpea-insert-default-filter
   (lambda (note)
     (= (vulpea-note-level note) 0))))



(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . adaptive-wrap-prefix-mode)
         ;; oh, how much I hate it in Org mode buffers
         (org-mode . editor-disable-electric-indent))
  :commands (org-check-agenda-file
             org-link-set-parameters)
  :init
  ;; This is where my ~heart~ org files are.
  (setq org-directory vulpea-directory)

  ;; Setup list of Org modules that should always be loaded together
  ;; with Org.
  (setq org-modules '(org-id org-attach))

  ;; pretty org files
  (setq
   org-adapt-indentation nil
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-image-actual-width '(512)
   org-imenu-depth 1
   org-pretty-entities nil
   org-startup-folded t)

  ;; do not allow invisible edits (...)
  (setq org-fold-catch-invisible-edits 'smart)

  ;; formatting for properties
  (setq org-property-format "%-24s %s")

  ;; setup todo keywords
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)"
               "HOLD(h@/!)"
               "|"
               "CANCELLED(c@/!)"
               "MEETING"))

   ;; use fast todo selection
   org-use-fast-todo-selection t

   ;; block parent until children are done
   org-enforce-todo-dependencies t

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
   org-log-into-drawer t)

  ;; tags
  (setq
   org-tag-persistent-alist '(("FOCUS" . ?f)
                              ("PROJECT" . ?p))
   org-use-tag-inheritance t
   org-tags-exclude-from-inheritance '("project"
                                       "litnotes"
                                       "people"))

  ;; avoid noisy `org-check-agenda-file'
  (advice-add #'org-check-agenda-file
              :around
              #'vulpea-check-agenda-file)
  :config
  ;; open directory links in `dired'
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; open files in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (setq org-indirect-buffer-display 'current-window))



(use-package org-persist
  :straight nil
  :defer t
  :commands (org-persist-gc
             org-persist-write-all)
  :config
  ;; disable caching upon killing emacs as it takes enormous amount of
  ;; time and instead run it from eru along with db sync
  (remove-hook 'kill-emacs-hook #'org-persist-write-all)
  (remove-hook 'kill-emacs-hook #'org-persist-gc))



(use-package org-clock
  :straight nil
  :defer t
  :commands (org-clock-save)
  :init
  (setq
   org-clock-persist-file (expand-file-name "org-clock-save.el"
                                            path-etc-dir)
   ;; remove clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist 'history
   ;; Resume when clocking into task with open clock
   org-clock-in-resume t)
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))



(use-package org-refile
  :straight nil
  :defer t
  :init
  (setq
   org-outline-path-complete-in-steps nil
   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))
   org-refile-use-outline-path t
   org-refile-allow-creating-parent-nodes nil
   org-refile-target-verify-function #'vulpea-refile-verify-target))



(use-package org-id
  :straight nil
  :defer t
  :hook ((before-save . vulpea-id-auto-assign)
         (org-capture-prepare-finalize . org-id-get-create))
  :init
  (setq org-id-uuid-program
        "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
  :config
  (org-link-set-parameters
   "id" :activate-func #'vulpea-activate-link)
  (setq
   org-id-track-globally t
   org-id-extra-files
   (list (expand-file-name ".archive/archive" org-directory)
         (expand-file-name ".archive/archive.org" org-directory))
   org-id-link-to-org-use-id t
   org-id-locations-file (expand-file-name "org-id-locations"
                                           path-cache-dir)))



(use-package org-capture
  :straight nil
  :defer t
  :general
  (leader-def
    "c" '(nil :which-key "capture...")
    "cX" '(org-capture :which-key "dispatch")
    "ca" '(vulpea-capture-article :which-key "article")
    "cj" '(vulpea-capture-journal :which-key "journal")
    "cl" '(org-store-link :which-key "link")
    "cm" '(vulpea-capture-meeting :which-key "meeting")
    "cx" '(vulpea-capture-task :which-key "task"))
  :init
  (setq-default org-capture-bookmark nil)
  :config
  (vulpea-capture-setup))



(use-package org-attach
  :straight nil
  :defer t
  :config
  (setq-default
   org-attach-id-dir (expand-file-name ".data/" vulpea-directory)
   org-attach-auto-tag nil
   org-attach-file-list-property nil
   org-attach-store-link-p 'attached))



(use-package org-archive
  :straight nil
  :defer t
  :init
  (setq-default
   org-archive-location
   (concat org-directory ".archive/%s_archive" "::" "datetree/*")
   org-archive-save-context-info
   '(time file ltags itags todo category olpath)))



(use-package org-agenda
  :straight nil
  :defer t
  :general
  (leader-def
    "oA" '(org-agenda :which-key "agenda dispatch")
    "oa" '(vulpea-agenda-main :which-key "agenda")
    "op" '(vulpea-agenda-person :which-key "person"))
  :config
  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (setq
   org-agenda-dim-blocked-tasks nil
   ;; setting it to t speeds up agenda, but... initial visibility is
   ;; not honored, which for me is a bigger issue
   org-agenda-inhibit-startup nil

   ;; also show state change in log mode
   org-agenda-log-mode-items '(closed clock state)

   ;; tags
   org-agenda-show-inherited-tags nil

   ;; more structured view
   org-agenda-prefix-format
   '((agenda . " %(vulpea-agenda-category 24) %?-12t %12s")
     (todo . " %(vulpea-agenda-category 24) ")
     (tags . " %(vulpea-agenda-category 24) ")
     (search . " %(vulpea-agenda-category 24) "))
   org-agenda-todo-keyword-format "%-1s"
   org-agenda-tags-column 0

   ;; show agenda in current window
   org-agenda-window-setup 'current-window

   org-agenda-custom-commands
   `((" " "Agenda"
      (,vulpea-agenda-cmd-refile
       ,vulpea-agenda-cmd-today
       ,vulpea-agenda-cmd-focus
       ,vulpea-agenda-cmd-waiting)
      ((org-agenda-buffer-name vulpea-agenda-main-buffer-name))))))



(use-package org-roam
  :defer t
  :commands (org-roam-db-autosync-enable
             org-roam-db-sync)
  :init
  (setq
   org-roam-v2-ack t
   org-roam-directory vulpea-directory
   org-roam-dailies-directory (expand-file-name
                               "journal/" org-roam-directory)
   org-roam-db-location (expand-file-name
                         (if vulpea-test-mode
                             "org-roam-test.db"
                           "org-roam.db")
                         path-cache-dir)
   org-roam-completion-everywhere t)
  :config
  ;; For some reason org-roam is loaded on init and twice! Suspect it
  ;; is related to the fact that I am loading it from different
  ;; branch.
  ;;
  ;; So since I need a quick remedy, I noop `org-roam-db-sync' during
  ;; setup, because I sync data base time to time from terminal via
  ;; eru.
  (advice-add #'org-roam-db-sync :around #'fun-noop)
  (ignore-errors
    (org-roam-db-autosync-enable))
  (advice-remove #'org-roam-db-sync #'fun-noop))



(use-package org-download
  :defer t
  :hook ((org-mode . org-download-enable))
  :init
  (setq-default org-download-method 'attach))



(use-package ox-latex
  :straight nil
  :defer t
  :config
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (setq org-preview-latex-image-directory
        (expand-file-name "ltximg/" path-cache-dir))
  (setq org-format-latex-options
        (list :foreground 'auto
              :background 'auto
              :scale 1.5
              :html-foreground "Black"
              :html-background "Transparent"
              :html-scale 1.0
              :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        (seq-map
         (lambda (_)
           (string-join '("pdflatex"
                          "-shell-escape"
                          "-interaction nonstopmode"
                          "-output-directory %o %f")
                        " "))
         '(1 2 3))))

(use-package ox-beamer
  :straight nil
  :defer t
  :config
  (add-to-list
   'org-beamer-environments-extra
   '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))



(use-package ob-plantuml
  :straight nil
  :defer t
  :defines (org-plantuml-jar-path))



(use-package toc-org
  :hook (org-mode . toc-org-mode))



(provide 'init-vulpea)
;;; init-vulpea.el ends here
