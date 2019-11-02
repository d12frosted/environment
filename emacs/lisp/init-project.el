;;; init-project.el --- project feature -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
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

(require 'init-keybindings)
(require 'init-path)

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :general
  (+leader-def
    "p" '(nil :which-key "project...")
    "pf" '(projectile-find-file :which-key "Find file in project")
    "p!" '(projectile-run-shell-command-in-root :which-key "Run cmd in project root")
    "pp" '(projectile-switch-project :which-key "Switch project")
    "pI" '(projectile-invalidate-cache :which-key "Invalidate cache")
    "p/" '(projectile-ripgrep :which-key "Grep the project"))
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
	      projectile-indexing-method 'alien
        projectile-cache-file (concat +path-cache-dir "projectile.cache")
        projectile-known-projects-file (concat +path-cache-dir "projectile-bookmarks.eld")))

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-root "projectile")

;;;###autoload
(defalias '+project-p #'projectile-project-p)

;;;###autoload
(defalias '+project-root #'projectile-project-root)

;;;###autoload
(defun +project-name (&optional dir)
  "Return the name of the current project.

By default current working directory is used, but you can specify
it manually by passing optional DIR argument."
  (let ((project-root (or (projectile-project-root dir)
                          (if dir (expand-file-name dir)))))
    (if project-root
        (funcall projectile-project-name-function project-root)
      "-")))

(provide 'init-project)
;;; init-project.el ends here
