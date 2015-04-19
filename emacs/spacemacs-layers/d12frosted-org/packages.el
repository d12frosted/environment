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
  '(
    ;; package d12frosted-orgs go here
    org
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-org-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted-org/init-org ()
  "Initialize org package."
  (use-package org
    :defer t

    :init

    :config
    (defvar d12frosted/org-home-path "~/Dropbox/org/")

    (defvar d12frosted/org-agenda-ignore-dirs
      (-map (lambda (dir) (d12frosted/concat-path d12frosted/org-home-path dir))
            '("temporary"
              "tmp")))

    (defvar d12frosted/org-agenda-dirs
      (d12frosted/directory-dirs d12frosted/org-home-path))

    (defvar d12frosted/org-agenda-files
      (-flatten (-map (lambda (dir) (d12frosted/org-files-in-folder dir)) d12frosted/org-agenda-dirs)))

    (defvar d12frosted/org-time-format
      "%H:%M:%S")

    (defvar d12frosted/org-date-format
      "%d %B %Y, %A")

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
          org-agenda-files d12frosted/org-agenda-files
          org-agenda-inhibit-startup nil)

    (define-key org-mode-map (kbd "C-c s o") 'd12frosted/org-sort-current-level)
    (define-key org-mode-map (kbd "C-c s O") 'd12frosted/org-sort-upper-level)
    (define-key org-mode-map (kbd "C-#") 'd12frosted/org-insert-block-template)

    (add-hook 'org-mode-hook 'd12frosted/org-auto-insert-template)))

;; For each package, define a function d12frosted-org/init-<package-d12frosted-org>
;;
;; (defun d12frosted-org/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
