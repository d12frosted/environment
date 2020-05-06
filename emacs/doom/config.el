;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Boris Buliga"
      user-mail-address "boris@d12frosted.io")

;;
;; Key bindings
(when IS-MAC
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))
(setq doom-leader-alt-key "M-m"
      doom-localleader-alt-key "M-m l")

;; hack to support M-m as leader key
(general-auto-unbind-keys)

(after! lispy
  (define-key lispy-mode-map (kbd "C-a") '+beginning-of-line))

(global-set-key [remap move-beginning-of-line]
                '+beginning-of-line)

(map! :leader
      (:prefix ("l" . "<localleader>"))

      :desc "Switch window"                "M-w" #'ace-window
      
      (:prefix-map ("b" . "buffer")
        (:when (featurep! :ui workspaces)
          :desc "Switch workspace buffer"    "b" #'persp-switch-to-buffer
          :desc "Switch buffer"              "B" #'switch-to-buffer)
        (:unless (featurep! :ui workspaces)
          :desc "Switch buffer"              "b" #'switch-to-buffer)
        :desc "Kill buffer"                  "d" #'kill-current-buffer
        :desc "Kill buffer"                  "k" #'kill-current-buffer
        :desc "Save buffer"                  "s" #'basic-save-buffer
        :desc "Revert buffer"                "r" #'revert-buffer
        :desc "Pop up scratch buffer"        "x" #'doom/open-scratch-buffer
        :desc "Switch to scratch buffer"     "X" #'doom/switch-to-scratch-buffer)

      (:prefix-map ("w" . "window")
        :desc "Split vertically"             "V" #'+window-split-vertically
        :desc "Split vertically and focus"   "v" #'+window-split-vertically-and-focus
        :desc "Split horizontally"           "H" #'split-window-vertically
        :desc "Split horizontally and focus" "h" #'split-window-vertically-and-focus
        :desc "Kill window"                  "k" #'delete-window
        :desc "Kill other windows"           "m" #'+window-zoom)

      (:prefix-map ("p" . "project")
        :desc "Search in project"            "/" #'+ivy/project-search
        :desc "Browse project"               "." #'+default/browse-project
        :desc "Browse other project"         ">" #'doom/browse-in-other-project
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
        :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
        :desc "Kill project buffers"         "k" #'projectile-kill-buffers
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Switch project"               "p" #'projectile-switch-project)

      (:prefix-map ("g" . "git")
        (:when (featurep! :tools magit)
          :desc "Magit dispatch"            "/"   #'magit-dispatch
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit status"              "g"   #'magit-status))

      (:prefix-map ("n" . "notes")
        :desc "Org agenda"                   "a" #'org-agenda
        :desc "List"                         "l" #'deft
        :desc "Find file"                    "f" #'org-roam-find-file
        :desc "Show graph"                   "g" #'org-roam-graph
        :desc "Insert"                       "i" #'org-roam-insert
        :desc "New journal entry"            "j" #'org-journal-new-entry
        (:prefix ("d" . "by date")
          :desc "Arbitrary date"             "d" #'org-roam-dailies-date
          :desc "Today"                      "t" #'org-roam-dailies-today
          :desc "Tomorrow"                   "m" #'org-roam-dailies-tomorrow
          :desc "Yesterday"                  "y" #'org-roam-dailies-yesterday))

      (:prefix ("j" . "jump to")
        :desc "Line (by number)"             "L" #'goto-line
        :desc "Line (avy)"                   "l" #'avy-goto-line
        :desc "Char"                         "j" #'avy-goto-char
        :desc "Chars"                        "J" #'avy-goto-char-timer
        :desc "Word"                         "w" #'avy-goto-word-0)

      (:prefix ("[" . "previous")
        (:when (featurep! :checkers spell)
          :desc "Spelling error"             "s" #'flyspell-correct-wrapper)))
