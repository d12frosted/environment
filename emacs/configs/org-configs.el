;;; org-configs.el --- configs file of org configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Configurable variables
;; ========================

(defvar d12/org-home-path "~/Dropbox/org/"
  "Directory where org files are stored.
   All org files in this directory and all subdirectories will be used as agenda
   files for org agenda. If you want to ignore some files, checkout
   'd12/org-agenda-ignore-dirs variable. Currently you can ignore files
   only by putting them into ignored directory.
   Warning! Don't forget to add trailing slash at the end.")

(defvar d12/org-agenda-ignore-dirs
  '("temporary/"
    "tmp/")
  "List of directories to be ignored for agenda. Every path should be relative
   to d12/org-home-path and must contain trailing slash.")

(defvar d12/org-time-format
  "%H:%M:%S"
  "Format for 'd12/org-insert-time function.")

(defvar d12/org-date-format
  "%d %B %Y, %A"
  "Format for 'd12/org-insert-date function.")

(defvar d12/org-default-title
  "Yet another org file"
  "Default title for org files. Is used by 'd12/org-guess-title
   when it failt to get the ttitle from buffer name.")

(defvar d12/org-author-name
  user-full-name
  "Author name (used in 'd12/org-new-file-template function.)")

(defvar d12/org-author-email
  user-mail-address
  "Author email (used in 'd12/org-new-file-template function.)")

;;; Configurations
;; ===============

(use-package org
  :ensure t
  :pin org
  :mode ("\\.org$" . org-mode)          ; todo - add org journal stuff as well
  :bind (("C-c o a"   . org-agenda)
         ("C-c o i t" . d12/org-insert-time)
         ("C-c o i d" . d12/org-insert-date)
         ("C-c o i D" . d12/org-insert-full-date)
         ("C-c o l" . org-store-link))
  :config
  (d12|define-prefix-global "C-c o i" insert)
  (require 's)

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
        org-directory d12/org-home-path
        org-agenda-inhibit-startup nil
        org-mobile-inbox-for-pull (concat d12/org-home-path "mobile.org")
        org-mobile-force-id-on-agenda-items nil
        org-mobile-directory "~/Dropbox/Apps/MobileOrg/")

  (require 'ox-publish)
  (setq org-html-htmlize-output-type 'inline-css
        org-html-validation-link nil
        org-publish-project-alist d12-blog/projects)

  (d12/reload-agenda-files)
  (d12|rename-modeline "org" org-mode "本")

  (bind-key "C-c c s" 'd12/org-sort-current-level org-mode-map)
  (bind-key "C-c c S" 'd12/org-sort-upper-level org-mode-map)
  (bind-key "C-c c #" 'd12/org-insert-block-template org-mode-map)

  ;; sometimes I am getting laze
  ;; but in this case - I am too lazy!
  ;; and this thing just a bizarre that RUINS MY LIFE!!!!
  ;; but I am too lazy to fix that
  ;; (add-hook 'org-mode-hook 'd12/org-auto-insert-template)
  )

(use-package org-indent
  :defer t
  :diminish org-indent-mode
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "✿" "❀" "✸")))

(use-package org-journal
  :ensure t
  :mode (".*/[0-9]*-[0-9]*-[0-9]*$" . org-journal-mode)
  :bind (("C-c o j"  . calendar)
         ("C-c o n"  . org-journal-new-entry)
         ("C-c o v"  . org-journal-visit-entry))
  :init
  (d12|rename-modeline "org-journal" org-journal-mode "日記")
  :config
  (global-unset-key (kbd "C-c C-j"))
  (setq org-journal-dir (concat d12/org-home-path "journal/")
        org-journal-date-format d12/org-date-format
        org-journal-time-format "%R\n"
        org-journal-file-format "%Y-%m-%d"
        org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
        org-journal-hide-entries-p nil))

;;; Publishing
;; ============

(defvar d12-blog/sources-path (concat d12/org-home-path "d12frosted/"))
(defvar d12-blog/publish-path "~/Developer/d12frosted.github.io/")

(defvar d12-blog/template-preamble (d12/get-string-from-file
                                    (concat d12-blog/sources-path
                                            "templates/preamble.html")))

(defvar d12-blog/template-head (d12/get-string-from-file
                                (concat d12-blog/sources-path
                                        "templates/head.html")))

(defvar d12-blog/projects '())
(defvar d12-blog/project-pages '())
(defvar d12-blog/project-static-files '())
(defvar d12-blog/project-posts '())
(defvar d12-blog/project-font-awesome '())
(defvar d12-blog/project-final '())

;;; Setup projects
;; ----------------

;; pages
(d12|plist-add d12-blog/project-pages :base-directory d12-blog/sources-path)
(d12|plist-add d12-blog/project-pages :publishing-directory d12-blog/publish-path)
(d12|plist-add d12-blog/project-pages :publishing-function 'org-html-publish-to-html)
(d12|plist-add d12-blog/project-pages :headline-levels 4)
(d12|plist-add d12-blog/project-pages :section-numbers nil)
(d12|plist-add d12-blog/project-pages :with-email t)
(d12|plist-add d12-blog/project-pages :html-head d12-blog/template-head)
(d12|plist-add d12-blog/project-pages :html-preamble d12-blog/template-preamble)
(d12|plist-add d12-blog/project-pages :html-postamble "")

;; static files
(d12|plist-add d12-blog/project-static-files :base-directory d12-blog/sources-path)
(d12|plist-add d12-blog/project-static-files :base-extension "png\\|jpg\\|gif\\|css\\|js")
(d12|plist-add d12-blog/project-static-files :publishing-directory d12-blog/publish-path)
(d12|plist-add d12-blog/project-static-files :publishing-function 'org-publish-attachment)
(d12|plist-add d12-blog/project-static-files :recursive t)

;; font-awesome
(d12|plist-add d12-blog/project-font-awesome :base-directory (concat d12-blog/sources-path "font-awesome/"))
(d12|plist-add d12-blog/project-font-awesome :base-extension ".*")
(d12|plist-add d12-blog/project-font-awesome :publishing-directory (concat d12-blog/publish-path "font-awesome/"))
(d12|plist-add d12-blog/project-font-awesome :publishing-function 'org-publish-attachment)
(d12|plist-add d12-blog/project-font-awesome :recursive t)

;; posts
(d12|plist-add d12-blog/project-posts :base-directory (concat d12-blog/sources-path "posts/"))
(d12|plist-add d12-blog/project-posts :publishing-directory (concat d12-blog/publish-path "posts/"))
(d12|plist-add d12-blog/project-posts :publishing-function 'org-html-publish-to-html)
(d12|plist-add d12-blog/project-posts :with-toc nil)
(d12|plist-add d12-blog/project-posts :headline-levels 4)
(d12|plist-add d12-blog/project-posts :section-numbers nil)
(d12|plist-add d12-blog/project-posts :html-head d12-blog/template-head)
(d12|plist-add d12-blog/project-posts :html-preamble d12-blog/template-preamble)
(d12|plist-add d12-blog/project-posts :html-postamble "<p>By %a<\p><p>Created on %d<\p>")
(d12|plist-add d12-blog/project-posts :auto-sitemap t)
(d12|plist-add d12-blog/project-posts :sitemap-function 'd12/org-publish-org-sitemap)
(d12|plist-add d12-blog/project-posts :sitemap-root d12-blog/sources-path)
(d12|plist-add d12-blog/project-posts :sitemap-filename "archive.org")
(d12|plist-add d12-blog/project-posts :sitemap-title "Archive")
(d12|plist-add d12-blog/project-posts :sitemap-style 'list)
(d12|plist-add d12-blog/project-posts :sitemap-sort-files 'anti-chronologically)
(d12|plist-add d12-blog/project-posts :sitemap-file-entry-format "%d - %t")

;; final
(d12|plist-add d12-blog/project-final :components '("d12-blog-pages"
                                                    "d12-blog-static-files"
                                                    "d12-blog-font-awesome"
                                                    "d12-blog-posts"))

;; projects list

(setq d12-blog/projects '())
(add-to-list 'd12-blog/projects (cons "d12-blog-pages" d12-blog/project-pages))
(add-to-list 'd12-blog/projects (cons "d12-blog-static-files" d12-blog/project-static-files))
(add-to-list 'd12-blog/projects (cons "d12-blog-font-awesome" d12-blog/project-font-awesome))
(add-to-list 'd12-blog/projects (cons "d12-blog-posts" d12-blog/project-posts))
(add-to-list 'd12-blog/projects (cons "d12-blog-final" d12-blog/project-final))
