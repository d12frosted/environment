;;; lang/vulpea/config.el -*- lexical-binding: t; -*-

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
