;;; packages.el --- d12frosted-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-org-packages
  '(org
    org-journal)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-org-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted-org/init-org ()
  "Initialize org package."
  (use-package org
    :defer 1
    :init
    :config

    (setq-local d12/org-ignored-dirs
                (-flatten
                 (-non-nil
                  (-map (lambda (dir)
                          (d12frosted/org-dir-and-subdirs dir))
                        d12frosted/org-agenda-ignore-dirs))))

    (setq-local d12/org-agenda-dirs
          (-difference (d12frosted/org-dir-and-subdirs "") d12/org-ignored-dirs))

    (setq-local d12/org-agenda-files
          (-flatten (-map (lambda (dir)
                            (d12frosted/org-files-in-folder dir))
                          d12/org-agenda-dirs)))

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

    (setq org-todo-keywords '((sequence "BLOCKED" "TODO" "DELAYED" "STARTED" "TEST" "|" "DONE" "PASS"))
          org-src-fontify-natively t
          org-directory d12frosted/org-home-path
          org-agenda-files d12/org-agenda-files
          org-agenda-inhibit-startup nil)

    (define-key org-mode-map (kbd "C-c o s") 'd12frosted/org-sort-current-level)
    (define-key org-mode-map (kbd "C-c o S") 'd12frosted/org-sort-upper-level)
    (define-key org-mode-map (kbd "C-c o #") 'd12frosted/org-insert-block-template)
    (define-key org-mode-map (kbd "C-c o d") 'd12frosted/org-insert-date)
    (define-key org-mode-map (kbd "C-c o t") 'd12frosted/org-insert-time)

    (spacemacs/declare-prefix "oj" "org/journal")

    (evil-leader/set-key "ojl" 'org-store-link)
    (evil-leader/set-key "oit" 'd12frosted/org-insert-time)
    (evil-leader/set-key "oid" 'd12frosted/org-insert-date)

    (add-hook 'org-mode-hook 'd12frosted/org-auto-insert-template)))

(defun d12frosted-org/init-org-journal ()
  "Initialize org-journal package"
  (use-package org-journal
    :defer 2
    :init
    :config

    (global-unset-key (kbd "C-c C-j"))

    (add-to-list 'auto-mode-alist '(".*/[0-9]*-[0-9]*-[0-9]*$" . org-mode))

    (evil-leader/set-key "ojc" 'calendar)
    (evil-leader/set-key "ojn" 'org-journal-new-entry)
    (evil-leader/set-key "ojv" 'org-journal-visit-entry)

    (setq org-journal-dir (s-concat d12frosted/org-home-path "journal/")
          org-journal-date-format "%d %B %Y, %A"
          org-journal-file-format "%Y-%m-%d"
          org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format))))
