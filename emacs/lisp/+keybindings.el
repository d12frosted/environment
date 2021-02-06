;;; +keybindings.el --- keybindings utils -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Oct 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-base)
(require 'init-keybindings)
(require 'init-editor)
(require 'init-org)

(global-set-key [M-S-down] #'move-text-down)
(global-set-key [M-S-up]   #'move-text-up)
(global-set-key [remap move-beginning-of-line] '+beginning-of-line)
(global-set-key (kbd "C-S-y") 'fancy-yank)
(global-set-key (kbd "<f12>") 'delve-open-or-select)

(+leader-def
  "C-w" '(ace-window :which-key "Ace window")
  "M-w" '(ace-window :which-key "Ace window")

  "/" '(nil :which-key "search...")

  "[" '(nil :which-key "previous...")
  "[s" '(flyspell-correct-wrapper :which-key "Spelling correction")

  "a" '(nil :which-key "align...")

  "c" '(nil :which-key "capture...")
  "cX" '(org-capture :which-key "dispatch...")
  "cj" '(+org-notes-new-journal-entry :which-key "journal")
  "cl" '(org-store-link :which-key "link")
  "cm" '(+org/capture-meeting :which-key "meeting")
  "cx" '(+org/capture-task :which-key "task")

  "g" '(nil :which-key "git...")
  "gS" '(magit-stage-file :which-key "stage file")
  "gU" '(magit-unstage-file :which-key "unstage file")
  "g[" '(git-gutter:previous-hunk :which-key "previous hunk")
  "g]" '(git-gutter:next-hunk :which-key "next hunk")
  "gd" '(magit-dispatch :which-key "dispatch")
  "gf" '(magit-find-file :which-key "find-file")
  "gg" '(magit-status :which-key "status")
  "gi" '(magit-init :which-key "initialize repo")
  "gt" '(git-timemachine-toggle :which-key "time machine")

  "i" '(nil :which-key "insert...")
  "iu" '(insert-char :which-key "Unicode character")

  "j" '(nil :which-key "jump...")
  "jJ" '(avy-goto-char-timer :which-key "Chars")
  "jL" '(goto-line :which-key "Line (by number)")
  "jb" '(ace-link :which-key "Button or link")
  "ji" '(imenu :which-key "imenu")
  "jj" '(avy-goto-char :which-key "Char")
  "jl" '(avy-goto-line :which-key "Line (avy)")
  "jw" '(avy-goto-word-0 :which-key "Word")

  "o" '(nil :which-key "open...")
  "oA" '(org-agenda :which-key "agenda dispatch")
  "oa" '(+agenda/main :which-key "agenda")
  "op" '(+agenda/person :which-key "person")
  "or" '(+orgability/list :which-key "reading list")
  "ow" '(+agenda/wix :which-key "work agenda")

  "n" '(nil :which-key "notes...")
  "nd" '(nil :which-key "by date...")
  "ndd" '(+org-notes-dailies-date :which-key "arbitrary date")
  "ndt" '(+org-notes-dailies-today :which-key "today")
  "ndn" '(+org-notes-dailies-next :which-key "next")
  "ndp" '(+org-notes-dailies-prev :which-key "previous")
  "nf" '(+org-notes-find :which-key "find")
  "nF" '(+org-notes-find-backlink :which-key "find backlink")
  "ng" '(org-roam-graph :which-key "graph")
  "ni" '(+org-notes-insert :which-key "insert")
  "nt" '(+org-notes-tags-add :which-key "tag")
  "nT" '(+org-notes-tags-delete :which-key "untag")
  "na" '(+org-notes-alias-add :which-key "alias")
  "nA" '(+org-notes-alias-delete :which-key "unalias")

  "v" '(nil :which-key "vino...")
  "vv" '(vino-entry-find-file :which-key "find vino")
  "vf" '(nil :which-key "find...")
  "vfa" '(vino-entry-find-file-available :which-key "available vino")
  "vfg" '(vino-grape-find-file :which-key "grape")
  "vfp" '(vino-producer-find-file :which-key "producer")
  "vfr" '(vino-region-find-file :which-key "region")
  "vn" '(vino-entry-create :which-key "create vino")
  "va" '(vino-entry-acquire :which-key "acquire vino")
  "vc" '(vino-entry-consume :which-key "consume vino")
  "vr" '(vino-entry-rate :which-key "rate vino")

  "b" '(nil :which-key "buffer...")
  "bM" '(+buffer/switch-to-messages :which-key "switch to messages buffer")
  "bS" '(+file/sudo-this :which-key "sudo edit this file")
  "bX" '(+buffer/switch-to-scratch :which-key "pop scratch buffer")
  "bb" '(switch-to-buffer :which-key "switch buffer")
  "bk" '(kill-this-buffer :which-key "kill buffer")
  "bm" '(+buffer/pop-messages :which-key "pop messages buffer")
  "bs" '(save-buffer :which-key "save buffer")
  "bx" '(+buffer/pop-scratch :which-key "pop scratch buffer")

  "w"  '(nil :which-key "window...")
  "wS" '(+window-split-horizontally :which-key "split frame horizontally")
  "wV" '(+window-split-vertically :which-key "split frame vertically")
  "wk" '(delete-window :which-key "kill window")
  "wm" '(+window-zoom :which-key "kill other windows")
  "ws" '(+window-split-horizontally-and-focus :which-key "split frame horizontally and focus")
  "wv" '(+window-split-vertically-and-focus :which-key "split frame vertically and focus")

  "p" '(nil :which-key "project...")
  "p!" '(+project-shell-command :which-key "Run cmd in project root")
  "p/" '(+project-find-regexp :which-key "Grep the project")
  "pf" '(+project-find-file :which-key "Find file in project")
  "pp" '(+project-switch :which-key "Switch project"))

(provide '+keybindings)
;;; +keybindings.el ends here
