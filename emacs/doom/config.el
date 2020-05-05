;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Boris Buliga"
      user-mail-address "boris@d12frosted.io")

(defconst +path-home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.

In a nutshell, it's just a value of $HOME.")

;;
;; UI
(setq doom-font (font-spec :family "Source Code Pro" :size 12)
      doom-variable-pitch-font (font-spec :family "Source Code Pro")
      doom-serif-font (font-spec :family "Source Code Pro")
      doom-theme 'leuven
      display-line-numbers-type nil)

(custom-theme-set-faces! 'leuven
  '(org-checkbox :background "#FAF7CC")
  '(doom-modeline-project-dir :foreground "SkyBlue3")
  '(doom-modeline-info :foreground "SkyBlue3")
  '(doom-modeline-buffer-modified :foreground "orange" :weight bold))

(when IS-MAC
  (setq-default line-spacing 1))

(after! org
  (set-popup-rule! "^CAPTURE"
    :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right :ttl 0))

(after! helpful
  (set-popup-rule! "^\\*helpful"
    :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right :ttl 0))

;;
;; org-mode
(setq org-directory (concat +path-home-dir "Dropbox/vulpea/")
      notes-directory (concat org-directory "notes/")

      ;; org-roam
      org-roam-directory notes-directory
      org-roam-graph-viewer (when IS-MAC "open")
      org-roam-graph-executable (executable-find "neato")
      org-roam-graph-extra-config '(("overlap" . "false"))
      org-roam-capture-templates
      '(("d" "default" plain #'org-roam-capture--get-point
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n#+TIME-STAMP: <>\n"
         :unnarrowed t))
      org-roam-dailies-capture-templates
      '(("d" "daily" plain #'org-roam-capture--get-point
         ""
         :immediate-finish t
         :file-name "%<%Y-%m-%d>"
         :head "#+TITLE: %<%A, %d %B %Y>\n#+TIME-STAMP: <>"))
      org-roam-graph-exclude-matcher '("literature_notes"
                                       "permanent_notes"
                                       "inbox"
                                       "unfinished"
                                       "people"
                                       "games"

                                       ;; very specific notes
                                       "20200401163827"
                                       "20200401163611"
                                       "20200401163758"
                                       "20200407181600"
                                       "20200430184542")

      ;; deft
      deft-directory notes-directory
      deft-recursive t
      deft-use-filter-string-for-filename t
      deft-default-extension "org"

      ;; org-journal
      org-journal-dir notes-directory
      org-journal-find-file #'find-file
      org-journal-hide-entries-p nil
      org-journal-date-prefix "#+TITLE: "
      org-journal-file-header "#+TIME-STAMP: <>"
      org-journal-time-prefix "* "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y")

(add-hook 'org-mode-hook #'auto-fill-mode)

(after! org-journal
  (set-company-backend! 'org-journal-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! deft
  (defun deft-parse-title (file _)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (require 'org-roam)
    (org-roam--get-title-or-slug file)))

;; TODO: make it org-roam only
(add-hook 'write-file-functions 'time-stamp)
(add-to-list 'window-buffer-change-functions
             #'+notes-setup-buffer)

;;;###autoload
(defun +notes-setup-buffer (&optional _)
  "Setup current buffer for notes viewing and editing.

If the current buffer is not a note, does nothing."
  (interactive)
  (unless (active-minibuffer-window)
    (if (and buffer-file-name
             (string-equal notes-directory
                           (file-name-directory buffer-file-name)))
        (progn
          (unless (bound-and-true-p org-roam-mode)
            (org-roam-mode 1))
          (setq-local time-stamp-start "#\\+TIME-STAMP:[ 	]+\\\\?[\"<]+")
          (setq org-roam-last-window (get-buffer-window))
          (unless (eq 'visible (org-roam--current-visibility))
            (delete-other-windows)
            (call-interactively #'org-roam)))
      (when (and (fboundp #'org-roam-buffer--visibility)
                 (eq 'visible (org-roam--current-visibility)))
        (delete-window (get-buffer-window org-roam-buffer))))))

;;;###autoload
(defun +notes-rebuild ()
  "Rebuild notes database."
  (interactive)
  (org-roam-db--clear)
  (org-roam-db-build-cache))

;;
;; emacs-lisp
(after! lispy
  (define-key lispy-mode-map (kbd "C-a") '+beginning-of-line))

;;
;; Key bindings
(when IS-MAC
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))
(setq doom-leader-alt-key "M-m"
      doom-localleader-alt-key "M-m l")

;; hack to support M-m as leader key
(general-auto-unbind-keys)
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

(global-set-key [remap move-beginning-of-line]
                '+beginning-of-line)

;;;###autoload
(defun +beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil, move forward ARG lines first. If point reaches
the beginning or end of the buffer, stop there."
  (interactive "P")
  (when (numberp arg)
    (let ((line-move-visual nil))
      (forward-line arg)))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun +window-split-vertically ()
  "Split window vertically."
  (interactive)
  (split-window-right))

;;;###autoload
(defun +window-split-vertically-and-focus ()
  "Split window vertically and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

;;;###autoload
(defun +window-split-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-below))

;;;###autoload
(defun +window-split-horizontally-and-focus ()
  "Split window horizontally and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

;;;###autoload
(defun +window-zoom ()
  "Close other windows to focus on this one.

Activate again to undo this. If the window changes before then,
the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))
