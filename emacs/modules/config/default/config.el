;;; config/default/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
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

(map! :leader
      :desc "Find file in project" "SPC" #'projectile-find-file
      :desc "Browse files" "." #'find-file
      (:when (featurep! :completion ivy)
	      :desc "Resume last search" "'" #'ivy-resume)

      (:desc "search" :prefix "/"
	      (:when (featurep! :completion ivy)
          :desc "Buffer" "b" #'swiper
          :desc "Project" "p" #'+ivy/project-search
          :desc "Directory" "d" #'+ivy/project-search-from-cwd)
	      :desc "Symbols" "i" #'imenu
        :desc "Symbols across buffers" "I" #'imenu-anywhere)

      (:desc "buffer" :prefix "b"
	      (:when (featurep! :feature workspaces)
          :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
          :desc "Switch buffer" "B" #'switch-to-buffer)
        (:unless (featurep! :feature workspaces)
          :desc "Switch buffer" "b" #'switch-to-buffer)
	      :desc "Kill buffer" "k" #'kill-this-buffer
	      :desc "Next buffer" "n" #'next-buffer
        :desc "Previous buffer" "p" #'previous-buffer
        :desc "Next buffer" "]" #'next-buffer
        :desc "Previous buffer" "[" #'previous-buffer
	      :desc "Save buffer" "s" #'save-buffer
	      :desc "Pop scratch buffer" "x" #'nucleus/open-scratch-buffer
	      :desc "Sudo edit this file" "S" #'+files/sudo-this)

      (:desc "file" :prefix "f"
        :desc "Find file" "." #'find-file
        :desc "Sudo find file" ">" #'+files/sudo-find
        :desc "Find file in project" "/" #'projectile-find-file
        :desc "Find file from here" "?" #'counsel-file-jump
        :desc "Find other file" "a" #'projectile-find-other-file
        :desc "Find directory" "d" #'dired
        :desc "Find file in .config" "e" #'+default/find-in-config
        :desc "Browse .config" "E" #'+default/browse-config
        :desc "Recent files" "r" #'recentf-open-files
        :desc "Recent project files" "R" #'projectile-recentf
        :desc "Yank filename" "y" #'+default/yank-buffer-filename
        :desc "Delete this file" "X" #'+files/delete-this)

      (:desc "git" :prefix "g"
        :desc "Magit blame" "b" #'magit-blame-addition
        :desc "Magit commit" "c" #'magit-commit
        :desc "Magit clone" "C" #'+magit/clone
        :desc "Magit dispatch" "d" #'magit-dispatch-popup
        :desc "Magit find-file" "f" #'magit-find-file
        :desc "Magit status" "g" #'magit-status
        :desc "Magit file delete" "x" #'magit-file-delete
        :desc "Initialize repo" "i" #'magit-init
        :desc "Browse issues tracker" "I" #'+vc/git-browse-issues
        :desc "Magit buffer log" "l" #'magit-log-buffer-file
        :desc "List repositories" "L" #'magit-list-repositories
        :desc "Browse remote" "o" #'+vc/git-browse
        :desc "Magit push popup" "p" #'magit-push-popup
        :desc "Magit pull popup" "P" #'magit-pull-popup
        :desc "Git revert hunk" "r" #'git-gutter:revert-hunk
        :desc "Git revert file" "R" #'vc-revert
        :desc "Git stage hunk" "s" #'git-gutter:stage-hunk
        :desc "Git stage file" "S" #'magit-stage-file
        :desc "Git time machine" "t" #'git-timemachine-toggle
        :desc "Git unstage file" "U" #'magit-unstage-file
        :desc "Next hunk" "]" #'git-gutter:next-hunk
        :desc "Previous hunk" "[" #'git-gutter:previous-hunk)

      (:desc "open" :prefix "o"
	:desc "Org agenda" "a" #'org-agenda)

      (:desc "project" :prefix "p"
        :desc "Browse project" "." #'+default/browse-project
        :desc "Find file in project" "/" #'projectile-find-file
        :desc "Run cmd in project root" "!" #'projectile-run-shell-command-in-root
        :desc "Compile project" "c" #'projectile-compile-project
        :desc "Find other file" "o" #'projectile-find-other-file
        :desc "Switch project" "p" #'projectile-switch-project
        :desc "Recent project files" "r" #'projectile-recentf
        :desc "List project tasks" "t" #'+ivy/tasks
        :desc "Invalidate cache" "x" #'projectile-invalidate-cache))

