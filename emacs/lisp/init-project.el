;;; init-project.el --- Project support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Feb 2021
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
;; Project management using built-in project.el with rg for fast searching.
;; Keybindings under M-m p.
;;
;;; Code:

(require 'init-elpa)
(require 'config-path)
(require 'lib-eval)



(defun project-shell-command ()
  "Invoke `shell-command' in the project's root."
  (interactive)
  (if-let* ((project (project-current))
            (root (project-root project)))
      (eval-with-default-dir root
        (call-interactively #'shell-command))
    (user-error "You are not in project")))

(use-package project
  :ensure nil
  :general
  (leader-def
    "p" '(nil :which-key "project...")
    "p!" '(project-shell-command :which-key "Run cmd in project root")
    "p/" '(project-find-regexp :which-key "Grep the project")
    "pf" '(project-find-file :which-key "Find file in project")
    "pp" '(project-switch :which-key "Switch project")
    "pc" '(project-compile :which-key "Compile project")))

(setq project-list-file (expand-file-name "projects" path-etc-dir))

(defalias 'project-switch #'project-switch-project)

(defun project-p ()
  "Return non-nil when located in a project."
  (project-current))

;; Emacs 27
(unless (fboundp 'project-root)
  (cl-defmethod project-root ((project (head transient)))
    (cdr project)))

(defun project-magit ()
  "Start `magit-status' in the current project's root directory."
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))

(setq project-switch-commands
      '((?f "Find file" project-find-file)
        (?g "Find regexp" project-find-regexp)
        (?d "Dired" project-dired)
        (?v "Magit" project-magit)
        (?e "Eshell" project-eshell)))

(use-package rg
  :ensure t
  :defer t
  :commands (rg-project)
  :init
  (defalias 'project-find-regexp #'rg-project))

(provide 'init-project)
;;; init-project.el ends here
