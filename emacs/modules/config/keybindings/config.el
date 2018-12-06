;;; config/keybindings/config.el -*- lexical-binding: t; -*-
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
      ;; most used key bindings
      :desc "Find file in project" "SPC" #'projectile-find-file
      :desc "Browse files" "." #'find-file

      ;; fast workspaces switch
      :desc "Switch to 1st workspace" "M-1" (λ! (+workspace/switch-to 0))
      :desc "Switch to 2nd workspace" "M-2" (λ! (+workspace/switch-to 1))
      :desc "Switch to 3th workspace" "M-3" (λ! (+workspace/switch-to 2))
      :desc "Switch to 4th workspace" "M-4" (λ! (+workspace/switch-to 3))
      :desc "Switch to 5th workspace" "M-5" (λ! (+workspace/switch-to 4))
      :desc "Switch to 6th workspace" "M-6" (λ! (+workspace/switch-to 5))
      :desc "Switch to 7th workspace" "M-7" (λ! (+workspace/switch-to 6))
      :desc "Switch to 8th workspace" "M-8" (λ! (+workspace/switch-to 7))
      :desc "Switch to 9th workspace" "M-9" (λ! (+workspace/switch-to 8))
      :desc "Switch to last workspace" "M-0" #'+workspace/switch-to-last

      (:when (featurep! :feature workspaces)
        :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
        :desc "Switch buffer" "<" #'switch-to-buffer)
      (:unless (featurep! :feature workspaces)
        :desc "Switch buffer" "," #'switch-to-buffer)

      (:when (featurep! :completion ivy)
	      :desc "Resume last search" "'" #'ivy-resume)

      (:desc "previous..." :prefix "["
        :desc "Buffer" "b" #'previous-buffer
        :desc "Diff Hunk" "d" #'git-gutter:previous-hunk
        :desc "Todo" "t" #'hl-todo-previous
        :desc "Error" "e" #'previous-error
        :desc "Workspace" "w" #'+workspace/switch-left
        :desc "Spelling correction" "s" #'flyspell-correct-wrapper)

      (:desc "next..." :prefix "]"
        :desc "Buffer" "b" #'next-buffer
        :desc "Diff Hunk" "d" #'git-gutter:next-hunk
        :desc "Todo" "t" #'hl-todo-next
        :desc "Error" "e" #'next-error
        :desc "Workspace" "w" #'+workspace/switch-right
        :desc "Spelling correction" "s" (λ! (let ((flyspell-correct--direction)) (flyspell-correct-wrapper 0))))

      (:desc "jump" :prefix "j"
        :desc "Line (by number)" "L" #'goto-line
        :desc "Line (avy)" "l" #'avy-goto-line
        :desc "Char" "j" #'avy-goto-char
        :desc "Chars" "J" #'avy-goto-char-timer
        :desc "Word" "w" #'avy-goto-word-0)

      (:desc "eval" :prefix "e"
        :desc "Eval buffer" "b" #'+eval/buffer
        :desc "Eval region" "e" #'+eval/region
        :desc "Eval region and replace" "r" #'+eval/region-and-replace)

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
	      :desc "Sudo edit this file" "S" #'+file/sudo-this)

      (:desc "file" :prefix "f"
        :desc "Find file" "." #'find-file
        :desc "Sudo find file" ">" #'+file/sudo-find
        :desc "Find file in project" "/" #'projectile-find-file
        :desc "Find file from here" "?" #'counsel-file-jump
        :desc "Find other file" "a" #'projectile-find-other-file
        :desc "Find directory" "d" #'dired
        :desc "Find file in .config" "c" #'+project/find-in-config
        :desc "Browse .config" "C" #'+project/browse-config
        :desc "Browse .config/emacs" "E" #'+project/browse-emacs-config
        :desc "Recent files" "r" #'recentf-open-files
        :desc "Recent project files" "R" #'projectile-recentf
        :desc "Yank filename" "y" #'+buffer/yank-filename
        :desc "Delete this file" "X" #'+file/delete-this)

      ;; TODO: revaluate these bindings
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
	      :desc "Org fast agenda" "a" #'+org/agenda-main
        :desc "Reading list" "r" #'+orgability/list
        :desc "Org agenda" "A" #'org-agenda
        :desc "Brain node" "b" #'org-brain-visualize
        :desc "REPL" "l" #'+eval/open-repl)

      (:desc "capture" :prefix "c"
        :desc "Org fast capture" "x" #'+org/capture-task
        :desc "Org fast capture" "m" #'+org/capture-meeting
        :desc "Org capture" "X" #'org-capture
        :desc "Orgability clip" "o" #'orgability-clip
        :desc "Org store link" "l" #'org-store-link)

      (:desc "project" :prefix "p"
        :desc "Browse project" "." #'+default/browse-project
        :desc "Find file in project" "/" #'projectile-find-file
        :desc "Run cmd in project root" "!" #'projectile-run-shell-command-in-root
        :desc "Compile project" "c" #'projectile-compile-project
        :desc "Find other file" "o" #'projectile-find-other-file
        :desc "Switch project" "p" #'projectile-switch-project
        :desc "Recent project files" "r" #'projectile-recentf
        :desc "List project tasks" "t" #'+ivy/tasks
        :desc "Invalidate cache" "x" #'projectile-invalidate-cache)

      :desc "Ace window" "M-w" #'ace-window
      (:desc "window" :prefix "w"
        :desc "Split frame vertically" "V" #'+window-split-vertically
        :desc "Split frame vertically and focus" "v" #'+window-split-vertically-and-focus
        :desc "Split frame horizontally" "S" #'+window-split-horizontally
        :desc "Split frame horizontally and focus" "s" #'+window-split-horizontally-and-focus
        :desc "Kill window" "k" #'delete-window
        :desc "Kill other windows" "m" #'delete-other-windows
        :desc "Ace window" "w" #'ace-window)

      (:desc "workspace" :prefix [tab]
        :desc "Display tab bar" [tab] #'+workspace/display
        :desc "New workspace" "n" #'+workspace/new
        :desc "Load workspace from file" "l" #'+workspace/load
        :desc "Load a past session" "L" #'+workspace/load-session
        :desc "Save workspace to file" "s" #'+workspace/save
        :desc "Autosave current session" "S" #'+workspace/save-session
        :desc "Switch workspace" "." #'+workspace/switch-to
        :desc "Kill all buffers" "x" #'+buffer/kill-all
        :desc "Delete session" "X" #'+workspace/kill-session
        :desc "Delete this workspace" "d" #'+workspace/delete
        :desc "Rename workspace" "r" #'+workspace/rename
        :desc "Restore last session" "R" #'+workspace/load-last-session
        :desc "Next workspace" "]" #'+workspace/switch-right
        :desc "Previous workspace" "[" #'+workspace/switch-left
        :desc "Switch to 1st workspace" "1" (λ! (+workspace/switch-to 0))
        :desc "Switch to 2nd workspace" "2" (λ! (+workspace/switch-to 1))
        :desc "Switch to 3rd workspace" "3" (λ! (+workspace/switch-to 2))
        :desc "Switch to 4th workspace" "4" (λ! (+workspace/switch-to 3))
        :desc "Switch to 5th workspace" "5" (λ! (+workspace/switch-to 4))
        :desc "Switch to 6th workspace" "6" (λ! (+workspace/switch-to 5))
        :desc "Switch to 7th workspace" "7" (λ! (+workspace/switch-to 6))
        :desc "Switch to 8th workspace" "8" (λ! (+workspace/switch-to 7))
        :desc "Switch to 9th workspace" "9" (λ! (+workspace/switch-to 8))
        :desc "Switch to last workspace" "0" #'+workspace/switch-to-last))
