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
(require 'init-package)
(require 'lib-fun)
(require 'project)

(defalias '+project-switch #'project-switch-project)
(defalias '+project-find-file #'project-find-file)
(defalias '+project-find-regexp #'project-find-regexp)

(defun +project-p ()
  "Return non-nil when located in a project."
  (project-current))

(defun +project-root ()
  "Return location of the current project."
  (when-let ((cur (project-current)))
    (cdr cur)))

(defun +project-shell-command ()
  "Invoke `shell-command' in the project's root."
  (interactive)
  (if-let ((root (+project-root)))
      (eval-with-default-dir root
        (call-interactively #'shell-command))
    (user-error "You are not in project")))

(use-package rg
  :defer t
  :commands (rg-project)
  :init
  (defalias '+project-find-regexp #'rg-project))

(provide 'init-project)
;;; init-project.el ends here
