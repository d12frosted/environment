;;; lang/vulpea/config.el -*- lexical-binding: t; -*-

(setq org-directory (concat +path-home-dir "Dropbox/vulpea/"))

(defvar +capture-inbox-file (format "inbox-%s.org" +sys-name)
  "The path to the inbox file.

It is relative to `org-directory', unless it is absolute.")

(defvar +capture-journal-file "journal.org"
  "The path to the journal file.

It is relative to `org-directory', unless it is absolute.")

(load! "+notes")

;;
;; org-id
;;

(setq
 ;; on macOS it's better to enforce lowercase, so my attachments are not
 ;; broken on other systems
 org-id-uuid-program "uuidgen | tr \"[:upper:]\" \"[:lower:]\""

 ;; create ids
 org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(add-hook! 'before-save-hook #'+org-auto-id-add-to-headlines-in-file)
(add-hook! 'org-capture-prepare-finalize-hook #'+org-auto-id-dwim)

;;
;; latex + beamer
;; 

(after! ox-latex
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (setq org-latex-listings 'minted)
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(after! ox-beamer
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))

;;
;; custom modes
;;

(use-package! org
  :hook ((org-mode . places-mode-maybe-enable)
         (org-mode . pretty-props-mode-maybe-enable)
         (org-mode . cha-mode-maybe-enable)
         (org-mode . wine-mode-maybe-enable)
         ;; (org-mode . cigars-mode-maybe-enable)
         (org-mode . +org/remove-empty-drawer)))

(use-package! org-brain
  :commands (org-brain-entry-from-id))

;;
;; orgability
;;

(use-package! orgability
  :defer t
  :init
  (setq
   orgability-file (expand-file-name "orgability.org" org-directory)
   orgability-agenda-topics-column 36))

(use-package! org-board
  :defer t
  :init
  (setq org-board-wget-show-buffer nil
        org-board-default-browser 'eww))

(use-package! org-download
  :hook (org-mode . org-download-enable)
  :init
  (setq-default org-download-method 'attach))

;;
;; Post initialisation
;;
;; Since most of the stuff is already configured by Doom in `org-load-hook', we
;; need to run our configurations in the end.
;;

(add-hook!
 'org-load-hook
 :append
 #'+org-post-init-h)

(defun +org-post-init-h ()
  "Run all late configurations for org."

  ;; tags
  (setq
   org-tag-persistent-alist '(("FOCUS" . ?f)
                              ("PROJECT" . ?p))
   org-tags-exclude-from-inheritance '("PROJECT"))

  ;; todo states
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)"
               "HOLD(h@/!)"
               "|"
               "CANCELLED(c@/!)"
               "MEETING"))

   ;; setup state triggers
   org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     ("HOLD" ("WAITING") ("HOLD" . t))
     (done ("WAITING") ("HOLD") ("FOCUS"))
     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

  ;; refile
  (setq org-refile-target-verify-function '+refile-verify-target)

  ;; archive
  (setq org-archive-location (concat org-directory
                                     ".archive/archive_%s"
                                     "::"
                                     "datetree/*"))

  ;; attachments
  (setq org-attach-id-dir ".data/")

  ;; state selection
  (setq
   ;; use fast todo selection
   org-use-fast-todo-selection t

   ;; allo to fast fix todo state without triggering anything
   org-treat-S-cursor-todo-selection-as-state-change nil

   ;; use drawer for state changes
   org-log-into-drawer t)

  ;; visuals
  (setq
   ;; my screen is not wide enough
   org-image-actual-width '(512)

   ;; do not allow invisible edits (...)
   org-catch-invisible-edits 'error

   ;; formatting
   org-property-format "%-24s %s"
   org-agenda-prefix-format
   '((agenda . " %i %-24:c %?-12t %12s")
     (todo . " %i %-24:c")
     (tags . " %i %-24:c")
     (search . " %i %-24:c"))
   org-agenda-todo-keyword-format "%-1s"
   org-agenda-tags-column 0

   ;; also show state change in log mode
   org-agenda-log-mode-items '(closed clock state))

  ;; capture
  (dolist (var '(+capture-inbox-file
                 +capture-journal-file))
    (unless (file-name-absolute-p (symbol-value var))
      (set var (expand-file-name (symbol-value var) org-directory))))
  (setq
   org-capture-templates
   '(("t" "todo" plain (file +capture-inbox-file)
      "* TODO %?\n%U\n" :clock-in t :clock-resume t)

     ("j" "Journal" entry (file+olp+datetree +capture-journal-file)
      "* %?\n%U\n" :clock-in t :clock-resume t :time-prompt t)

     ("n" "note" entry (file +capture-inbox-file)
      "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

     ("m" "Meeting" entry (file +capture-inbox-file)
      "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))
