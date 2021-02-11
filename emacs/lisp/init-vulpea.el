;;; init-vulpea.el --- Note taking and project management  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 08 Feb 2021
;;
;; URL: https://github.com/d12frosted/
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

(require 'init-elpa)
(require 'lib-vulpea)



(use-package vulpea
  :quelpa (vulpea
           :fetcher github
           :repo "d12frosted/vulpea")
  :defer t)



(use-package org
  :min-version "9.4.4"
  :hook ((org-mode . auto-fill-mode)
         ;; oh, how much I hate it in Org mode buffers
         (org-mode . editor-disable-electric-indent))
  :init
  ;; This is where my ~heart~ org files are.
  (setq org-directory vulpea-directory)

  ;; Setup list of Org modules that should always be loaded together
  ;; with Org.
  (setq org-modules
        '(org-agenda
          org-archive
          org-capture
          org-refile
          org-id
          org-attach))

  ;; pretty org files
  (setq
   org-adapt-indentation nil
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-image-actual-width '(512)
   org-imenu-depth 1
   org-pretty-entities nil
   org-startup-folded t
   org-startup-indented t)

  ;; do not allow invisible edits (...)
  (setq org-catch-invisible-edits 'error)

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
   org-tags-exclude-from-inheritance '("PROJECT"))
  :config
  ;; open directory links in `dired'
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; open files in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (setq org-indirect-buffer-display 'current-window))



(use-package org-clock
  :built-in t
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
  :built-in t
  :defer t
  :init
  (setq
   org-outline-path-complete-in-steps nil
   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))
   org-refile-use-outline-path t
   org-refile-allow-creating-parent-nodes nil
   org-refile-target-verify-function #'vulpea-refile-verify-target))



(use-package org-indent
  :built-in t
  :defer t
  :diminish org-indent-mode
  :hook ((org-mode . org-indent-mode)))



(use-package org-id
  :built-in t
  :defer t
  :hook ((before-save . vulpea-id-auto-assign)
         (org-capture-prepare-finalize . org-id-get-create))
  :init
  (setq org-id-uuid-program
        "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
  :config
  (setq
   org-id-track-globally t
   org-id-extra-files
   (list (expand-file-name ".archive/archive" org-directory)
         (expand-file-name ".archive/archive.org" org-directory))
   org-id-link-to-org-use-id t
   org-id-locations-file (expand-file-name "org-id-locations"
                                           path-cache-dir)))

(provide 'init-vulpea)
;;; init-vulpea.el ends here
