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
(require 'config-vulpea)



(use-package vulpea
  :ensure (:host github :repo "d12frosted/vulpea")
  :defer t
  :general
  (leader-def
    "f" '(nil :which-key "find...")
    "fa" '(vulpea-find-area :which-key "area")
    "ft" '(vulpea-find-project :which-key "project")
    "fp" '(vulpea-find-person :which-key "person")
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
    "nA" '(vulpea-alias-delete :which-key "unalias"))
  :hook ((before-save . vulpea-pre-save-hook)
         (org-roam-db-autosync-mode . vulpea-db-setup-attachments)
         (org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  :init
  (setq-default
   vulpea-find-default-candidates-source #'vulpea-find-candidates
   vulpea-insert-default-candidates-source #'vulpea-insert-candidates
   vulpea-find-default-filter
   (lambda (note)
     (= (vulpea-note-level note) 0))
   vulpea-insert-default-filter
   (lambda (note)
     (= (vulpea-note-level note) 0)))
  :config
  (add-hook 'vulpea-insert-handle-functions
            #'vulpea-insert-handle)

  ;; This must be configured in config hook to avoid unnecessary load
  ;; of `vulpea' stuff.
  (add-to-list 'window-buffer-change-functions
               #'vulpea-setup-buffer))



(use-package org
  :ensure (org :host sourcehut :repo "bzg/org-mode")
  :hook ((org-mode . visual-line-mode)
         (org-mode . adaptive-wrap-prefix-mode)
         ;; I like it, but it's too buggy
         ;; (org-mode . org-indent-mode)
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
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-image-actual-width '(512)
   org-imenu-depth 1
   org-pretty-entities nil
   org-startup-folded 'show2levels
   ;; I like it, but it's too buggy
   org-fontify-whole-heading-line nil
   org-blank-before-new-entry '((heading . t)
                                (plain-list-item . nil))
   org-cycle-separator-lines 1)

  ;; do not allow invisible edits (...)
  (setq-default org-fold-catch-invisible-edits 'smart)

  ;; formatting for properties
  (setq org-property-format "%-24s %s")

  ;; setup todo keywords
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence
      "WAITING(w@/!)"
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
                                       "area"
                                       "litnotes"
                                       "FOCUS"
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
  (setq org-indirect-buffer-display 'current-window)

  ;; Use RET to open org-mode links, including those in quick-help.org
  (setq org-return-follows-link t))



(use-package org-persist
  :ensure nil
  :after org
  :commands (org-persist-gc
             org-persist-write-all)
  :config
  ;; disable caching upon killing emacs as it takes enormous amount of
  ;; time and instead run it from eru along with db sync
  (remove-hook 'kill-emacs-hook #'org-persist-write-all)
  (remove-hook 'kill-emacs-hook #'org-persist-gc))



(use-package org-clock
  :ensure nil
  :after org
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
  :ensure nil
  :after org
  :init
  (setq
   org-outline-path-complete-in-steps nil
   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))
   org-refile-use-outline-path t
   org-refile-allow-creating-parent-nodes nil
   org-refile-target-verify-function #'vulpea-refile-verify-target))



(use-package org-id
  :ensure nil
  :after org
  :hook ((before-save . vulpea-id-auto-assign))
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



(use-package org-capture
  :ensure nil
  :after org
  :general
  (leader-def
    "c" '(nil :which-key "capture...")
    "cX" '(org-capture :which-key "dispatch")
    "ca" '(vulpea-capture-area :which-key "area")
    "cj" '(vulpea-capture-journal :which-key "journal")
    "cl" '(org-store-link :which-key "link")
    "cm" '(vulpea-capture-meeting :which-key "meeting")
    "cp" '(vulpea-capture-project :which-key "project")
    "cx" '(vulpea-capture-task :which-key "task"))
  :init
  (setq-default org-capture-bookmark nil)
  :config
  (vulpea-capture-setup))



(use-package org-attach
  :ensure nil
  :after org
  :config
  (setq-default
   org-attach-id-dir (expand-file-name ".data/" vulpea-directory)
   org-attach-auto-tag nil
   org-attach-file-list-property nil
   org-attach-store-link-p 'attached))



(use-package org-archive
  :ensure nil
  :after org
  :init
  (setq-default
   org-archive-location
   (concat org-directory ".archive/%s_archive" "::" "datetree/*")
   org-archive-save-context-info
   '(time file ltags itags todo category olpath)))



(use-package org-agenda
  :ensure nil
  :after org
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
   '((agenda . " %(vulpea-agenda-category 36) %?-12t %12s")
     (todo . " %(vulpea-agenda-category 36) ")
     (tags . " %(vulpea-agenda-category 36) ")
     (search . " %(vulpea-agenda-category 36) "))
   org-agenda-todo-keyword-format "%-1s"
   org-agenda-tags-column 0

   ;; show agenda in current window
   org-agenda-window-setup 'current-window

   ;; these tags have no value in agenda buffer
   org-agenda-hide-tags-regexp "REFILE\\|FOCUS\\|MEETING"

   org-agenda-custom-commands
   `((" " "Agenda"
      (,vulpea-agenda-cmd-refile
       ,vulpea-agenda-cmd-today
       ,vulpea-agenda-cmd-focus
       ,vulpea-agenda-cmd-waiting)
      ((org-agenda-buffer-name vulpea-agenda-main-buffer-name))))))



(use-package org-roam
  :ensure t
  :defer t
  :commands (org-roam-db-autosync-enable
             org-roam-db-sync)
  :init
  (setq
   org-roam-v2-ack t
   org-roam-directory vulpea-directory
   org-roam-dailies-directory (expand-file-name
                               "journal/" org-roam-directory)
   org-roam-database-connector 'sqlite-builtin
   org-roam-db-location (expand-file-name
                         (if vulpea-test-mode
                             "org-roam-test.db"
                           "org-roam.db")
                         path-cache-dir)
   org-roam-completion-everywhere nil
   ;; remove this atrocity from save-hook, it eats too much CPU on
   ;; buffers with lots of links and serves me zero purpose.
   org-roam-link-auto-replace nil)
  :config
  (unless elpa-bootstrap-p
    (org-roam-db-autosync-enable)))



(use-package org-download
  :ensure t
  :defer t
  :hook ((org-mode . org-download-enable))
  :init
  (setq-default
   org-download-method 'attach
   org-download-annotate-function (lambda (_) "")
   org-download-file-format-function #'identity))



(use-package ox-latex
  :ensure nil
  :after org
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
  (setq org-latex-src-block-backend 'minted)
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
  :ensure nil
  :after org
  :config
  (add-to-list
   'org-beamer-environments-extra
   '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))



(use-package ob-plantuml
  :ensure nil
  :defer t
  :defines (org-plantuml-jar-path))



(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))



(use-package org-ml
  :ensure t
  :defer t)



(use-package fancy-yank
  :ensure (:host github :repo "d12frosted/fancy-yank")
  :commands (fancy-yank)
  :bind
  (("C-S-y" . #'fancy-yank))
  :config
  (setq
   fancy-yank-rules
   (list
    (cons vcs-url-github-issue-regexp
          '(fancy-yank-extract-regex
            (lambda (url owner repo type number &rest args)
              (list url
               (vcs-url-format-github-issue
                owner repo type number)))
            fancy-yank-format-link))
    (cons vcs-url-github-project-regexp
          '(fancy-yank-extract-regex
            (lambda (url owner repo &rest args)
              (list url
               (vcs-url-format-github-project owner repo)))
            fancy-yank-format-link))
    (cons (format "\\(https?://%s/package/\\([-[:alnum:]]+\\).*\\)"
                  "hackage.haskell.org")
          '(fancy-yank-extract-regex
            (lambda (url package &rest args)
              (list
               url
               package))
            fancy-yank-format-link))
    (cons string-http-url-regexp
          '(fancy-yank-extract-regex
            (lambda (url &rest args)
              (list
               url
               (or (ignore-errors (url-domain
                                   (url-generic-parse-url url)))
                (read-string "Description: "))))
            fancy-yank-format-link)))))

(use-package org-cliplink
  :ensure t
  :defer t)



(provide 'init-vulpea)
;;; init-vulpea.el ends here
