;;; packages.el --- d12frosted Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-packages
  '(
    ;; package d12frosteds go here
    org
    omnisharp
    csharp-mode
    company)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted/init-org ()
  "Initialize org package."
  (use-package org
    :defer t

    :init

    :config
    (defvar d12frosted/org-home-path "~/Dropbox/org")

    (defvar d12frosted/org-agenda-ignore-dirs
      (-map (lambda (dir) (d12frosted/concat-path d12frosted/org-home-path dir))
            '("temporary"
              "tmp")))

    (defvar d12frosted/org-agenda-dirs
      (d12frosted/directory-dirs d12frosted/org-home-path))

    (defvar d12frosted/org-agenda-files
      (-flatten (-map (lambda (dir) (d12frosted/org-files-in-folder dir)) d12frosted/org-agenda-dirs)))

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

    (add-hook 'org-mode-hook 'd12frosted/org-auto-insert-template)))

(defun d12frosted/init-company ()
  "Initialize org-mode package."
  (use-package company
    :defer 1
    :init
    :config
    ))

(defun d12frosted/init-csharp-mode ()
  "Initialize csharp-mode package."
  (use-package csharp-mode
    :defer 2
    :init
    :config
    ))

(defun d12frosted/init-omnisharp ()
  "Initialize omnisharp package."
  (use-package omnisharp
    :defer 3
    :init
    :config
    ))

;; For each package, define a function d12frosted/init-<package-d12frosted>
;;
;; (defun d12frosted/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
