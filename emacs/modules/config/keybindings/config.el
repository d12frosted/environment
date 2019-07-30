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

;; Load the mighty general
(require 'general)

;; Setup escape
(global-set-key [remap keyboard-quit] #'+escape)

(general-create-definer +leader-def
  :states nil
  :keymaps 'override
  :prefix "M-m"
  :prefix-command 'nucleus-prefix-command
  :prefix-map 'nucleus-prefix-map)

(+leader-def
  "" nil

  ;; file navigation
  "SPC" '(projectile-find-file :which-key "Find file in project")
  "." '(find-file :which-key "Browse files")

  ;; fast workspaces switch
  "M-1" '((λ! (+workspace/switch-to 0)) :which-key "Switch to 1st workspace")
  "M-2" '((λ! (+workspace/switch-to 1)) :which-key "Switch to 2nd workspace")
  "M-3" '((λ! (+workspace/switch-to 2)) :which-key "Switch to 3th workspace")
  "M-4" '((λ! (+workspace/switch-to 3)) :which-key "Switch to 4th workspace")
  "M-5" '((λ! (+workspace/switch-to 4)) :which-key "Switch to 5th workspace")
  "M-6" '((λ! (+workspace/switch-to 5)) :which-key "Switch to 6th workspace")
  "M-7" '((λ! (+workspace/switch-to 6)) :which-key "Switch to 7th workspace")
  "M-8" '((λ! (+workspace/switch-to 7)) :which-key "Switch to 8th workspace")
  "M-9" '((λ! (+workspace/switch-to 8)) :which-key "Switch to 9th workspace")
  "M-0" '(+workspace/switch-to-last :which-key "Switch to last workspace")

  "M-w" '(ace-window :which-key "Ace window")

  "," '(persp-switch-to-buffer :which-key "Switch workspace buffer")
  "<" '(switch-to-buffer :which-key "Switch buffer")

  "'" '(ivy-resume :which-key "Resume last search")

  "[" '(nil :which-key "previous...")
  "[b" '(previous-buffer :which-key "Buffer")
  "[d" '(git-gutter:previous-hunk :which-key "Diff Hunk")
  "[t" '(hl-todo-previous :which-key "Todo")
  "[e" '(previous-error :which-key "Error")
  "[w" '(+workspace/switch-left :which-key "Workspace")
  "[s" '(flyspell-correct-wrapper :which-key "Spelling correction")

  "]" '(nil :which-key "next...")
  "]b" '(next-buffer :which-key "Buffer")
  "]d" '(git-gutter:next-hunk :which-key "Diff Hunk")
  "]t" '(hl-todo-next :which-key "Todo")
  "]e" '(next-error :which-key "Error")
  "]w" '(+workspace/switch-right :which-key "Workspace")
  "]s" '((λ! (let ((flyspell-correct--direction)) (flyspell-correct-wrapper 0))) "Spelling correction")

  "j" '(nil :which-key "jump...")
  "jL" '(goto-line :which-key "Line (by number)")
  "jl" '(avy-goto-line :which-key "Line (avy)")
  "jj" '(avy-goto-char :which-key "Char")
  "jJ" '(avy-goto-char-timer :which-key "Chars")
  "jw" '(avy-goto-word-0 :which-key "Word")

  "e" '(nil :which-key "eval...")
  "eb" '(+eval/buffer :which-key "Eval buffer")
  "ee" '(+eval/region :which-key "Eval region")
  "er" '(+eval/region-and-replace :which-key "Eval region and replace")

  "/" '(nil :which-key "search...")
  "/b" '(swiper :which-key "Buffer")
  "/p" '(+ivy/project-search :which-key "Project")
  "/d" '(+ivy/project-search-from-cwd :which-key "Directory")
  "/i" '(imenu :which-key "Symbols")
  "/I" '(imenu-anywhere :which-key "Symbols across buffers")

  "b" '(nil :which-key "buffer...")
  "bb" '(persp-switch-to-buffer :which-key "Switch workspace buffer")
  "bB" '(switch-to-buffer :which-key "Switch buffer")
  "bk" '(kill-this-buffer :which-key "Kill buffer")
  "bn" '(next-buffer :which-key "Next buffer")
  "bp" '(previous-buffer :which-key "Previous buffer")
  "b]" '(next-buffer :which-key "Next buffer")
  "b[" '(previous-buffer :which-key "Previous buffer")
  "bs" '(save-buffer :which-key "Save buffer")
  "bx" '(+buffer/pop-scratch :which-key "Pop scratch buffer")
  "bX" '(+buffer/switch-to-scratch :which-key "Pop scratch buffer")
  "bm" '(+buffer/pop-messages :which-key "Pop messages buffer")
  "bM" '(+buffer/switch-to-messages :which-key "Switch to messages buffer")
  "bS" '(+file/sudo-this :which-key "Sudo edit this file")

  "f" '(nil :which-key "file...")
  "f." '(find-file :which-key "Find file")
  "f>" '(+file/sudo-find :which-key "Sudo find file")
  "f/" '(projectile-find-file :which-key "Find file in project")
  "f?" '(counsel-file-jump :which-key "Find file from here")
  "fa" '(projectile-find-other-file :which-key "Find other file")
  "fd" '(dired :which-key "Find directory")
  "fc" '(+project/find-in-config :which-key "Find file in .config")
  "fC" '(+project/browse-config :which-key "Browse .config")
  "fE" '(+project/browse-emacs-config :which-key "Browse .config/emacs")
  "fr" '(recentf-open-files :which-key "Recent files")
  "fR" '(projectile-recentf :which-key "Recent project files")
  "fy" '(+buffer/yank-filename :which-key "Yank filename")
  "fX" '(+file/delete-this :which-key "Delete this file")

  "g" '(nil :which-key "git...")
  "gb" '(magit-blame-addition :which-key "Magit blame")
  "gc" '(magit-commit :which-key "Magit commit")
  "gC" '(+magit/clone :which-key "Magit clone")
  "gd" '(magit-dispatch-popup :which-key "Magit dispatch")
  "gf" '(magit-find-file :which-key "Magit find-file")
  "gg" '(magit-status :which-key "Magit status")
  "gx" '(magit-file-delete :which-key "Magit file delete")
  "gi" '(magit-init :which-key "Initialize repo")
  "gI" '(+vc/git-browse-issues :which-key "Browse issues tracker")
  "gl" '(magit-log-buffer-file :which-key "Magit buffer log")
  "gL" '(magit-list-repositories :which-key "List repositories")
  "go" '(+vc/git-browse :which-key "Browse remote")
  "gp" '(magit-push-popup :which-key "Magit push popup")
  "gP" '(magit-pull-popup :which-key "Magit pull popup")
  "gr" '(git-gutter:revert-hunk :which-key "Git revert hunk")
  "gR" '(vc-revert :which-key "Git revert file")
  "gs" '(git-gutter:stage-hunk :which-key "Git stage hunk")
  "gS" '(magit-stage-file :which-key "Git stage file")
  "gt" '(git-timemachine-toggle :which-key "Git time machine")
  "gU" '(magit-unstage-file :which-key "Git unstage file")
  "g]" '(git-gutter:next-hunk :which-key "Next hunk")
  "g[" '(git-gutter:previous-hunk :which-key "Previous hunk")

  "o" '(nil :which-key "open...")
  "oa" '(+agenda/main :which-key "Org fast agenda")
  "ow" '(+agenda/wix :which-key "Org Wix fast agenda")
  "or" '(+orgability/list :which-key "Reading list")
  "oA" '(org-agenda :which-key "Org agenda")
  "ob" '(org-brain-visualize :which-key "Brain node")
  "ol" '(+eval/open-repl :which-key "REPL")

  "c" '(nil :which-key "capture...")
  "cj" '(+org/capture-journal :which-key "Org fast capture")
  "cm" '(+org/capture-meeting :which-key "Org fast capture")
  "cn" '(+org/capture-note :which-key "Org fast capture")
  "cx" '(+org/capture-task :which-key "Org fast capture")
  "cX" '(org-capture :which-key "Org capture")
  "co" '(orgability-clip :which-key "Orgability clip")
  "cl" '(org-store-link :which-key "Org store link")

  "p" '(nil :which-key "project...")
  "p." '(+default/browse-project :which-key "Browse project")
  "p/" '(projectile-find-file :which-key "Find file in project")
  "p!" '(projectile-run-shell-command-in-root :which-key "Run cmd in project root")
  "pc" '(projectile-compile-project :which-key "Compile project")
  "po" '(projectile-find-other-file :which-key "Find other file")
  "pp" '(projectile-switch-project :which-key "Switch project")
  "pr" '(projectile-recentf :which-key "Recent project files")
  "pt" '(+ivy/tasks :which-key "List project tasks")
  "px" '(projectile-invalidate-cache :which-key "Invalidate cache")

  "w" '(nil :which-key "window...")
  "wV" '(+window-split-vertically :which-key "Split frame vertically")
  "wv" '(+window-split-vertically-and-focus :which-key "Split frame vertically and focus")
  "wS" '(+window-split-horizontally :which-key "Split frame horizontally")
  "ws" '(+window-split-horizontally-and-focus :which-key "Split frame horizontally and focus")
  "wk" '(delete-window :which-key "Kill window")
  "wm" '(+window-zoom :which-key "Kill other windows")
  "ww" '(ace-window) :which-key "Ace window"

  "TAB" '(nil :which-key "workspace...")
  "TAB TAB" '(+workspace/display :which-key "Display tab bar")
  "TAB n" '(+workspace/new :which-key "New workspace")
  "TAB l" '(+workspace/load :which-key "Load workspace from file")
  "TAB L" '(+workspace/load-session :which-key "Load a past session")
  "TAB s" '(+workspace/save :which-key "Save workspace to file")
  "TAB S" '(+workspace/save-session :which-key "Autosave current session")
  "TAB ." '(+workspace/switch-to :which-key "Switch workspace")
  "TAB x" '(+buffer/kill-all :which-key "Kill all buffers")
  "TAB X" '(+workspace/kill-session :which-key "Delete session")
  "TAB d" '(+workspace/delete :which-key "Delete this workspace")
  "TAB r" '(+workspace/rename :which-key "Rename workspace")
  "TAB R" '(+workspace/load-last-session :which-key "Restore last session")
  "TAB ]" '(+workspace/switch-right :which-key "Next workspace")
  "TAB [" '(+workspace/switch-left :which-key "Previous workspace")
  "TAB 1" '((λ! (+workspace/switch-to 0)) :which-key "Switch to 1st workspace")
  "TAB 2" '((λ! (+workspace/switch-to 1)) :which-key "Switch to 2nd workspace")
  "TAB 3" '((λ! (+workspace/switch-to 2)) :which-key "Switch to 3rd workspace")
  "TAB 4" '((λ! (+workspace/switch-to 3)) :which-key "Switch to 4th workspace")
  "TAB 5" '((λ! (+workspace/switch-to 4)) :which-key "Switch to 5th workspace")
  "TAB 6" '((λ! (+workspace/switch-to 5)) :which-key "Switch to 6th workspace")
  "TAB 7" '((λ! (+workspace/switch-to 6)) :which-key "Switch to 7th workspace")
  "TAB 8" '((λ! (+workspace/switch-to 7)) :which-key "Switch to 8th workspace")
  "TAB 9" '((λ! (+workspace/switch-to 8)) :which-key "Switch to 9th workspace")
  "TAB 0" '(+workspace/switch-to-last :which-key "Switch to last workspace")
  )

(def-package! which-key
  :defer 1
  :after-call pre-command-hook
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)
  (which-key-mode +1))

;; `hydra'
(setq lv-use-seperator t)
