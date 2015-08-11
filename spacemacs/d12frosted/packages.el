;;; packages.el --- d12frosted Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Boris Buliga & Contributors
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq d12frosted-packages
      '(
        org
        org-journal
        omnisharp
        elfeed
        elfeed-web
        ))

;; List of packages to exclude.
(setq d12frosted-excluded-packages '())

;; =========================
;;; General helper functions
;; =========================

;; ----------------------
;;; Files and directories
;; ----------------------

(defun d12/directory-dirs (directory)
  "Return a list of names of directories in DIRECTORY
  excluding '.' and '..'."
  (unless (file-directory-p directory)
    (error "Not a directory `%s'" directory))
  (let* ((dir (directory-file-name directory))
         (dirs '())
         (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file "/")))
          (when (file-directory-p file)
            (add-to-list 'dirs file)))))
    dirs))

(defun d12/directory-dirs-r (directory)
  "Return a list of names of directories in DIRECTORY
  and all it's subdirectories excluding '.' and '..'."
  (let ((dirs '()))
    (dolist (dir (d12/directory-dirs directory))
      (setq dirs (append (cons dir
                               (d12/directory-dirs-r dir))
                         dirs)))
    dirs))

(defun d12/get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun buffer-contains-substring? (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

;; -----------
;;; Navigation
;; -----------

(defun d12/goto-line-and-center ()
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

;; ------------------------------
;;; Mode renaming and diminishing
;; ------------------------------

(defmacro d12|rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after d12|rename-modeline-hack activate)
        (setq mode-name ,new-name))))

(defmacro d12|diminish (mode dim)
  "Diminish MODE name in mode line to DIM."
  `(eval-after-load 'diminish '(diminish ',mode ,dim)))

;; deprecated
(defmacro d12|lazy-diminish (mode dim)
  "Diminish MODE name in mode line to DIM after PACKAGE-NAME is loaded."
  `(defadvice ,mode (after d12|lazy-diminish-hack activate)
     (d12|diminish ,mode ,dim)))

;; -------------------
;;; Text manipulations
;; -------------------

(defun d12/copy-line-or-region ()
  "Copy current line (with newline character) or region. When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (d12/line-or-region-point-min)
                  (d12/line-or-region-point-max))
  (message "copied"))

(defun d12/cut-line-or-region ()
  "Cut current line or region. When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-region (d12/line-or-region-point-min)
               (d12/line-or-region-point-max))
  (message "cut"))

(defun d12/duplicate-line-or-region ()
  "Duplicates current line or region. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (d12/line-or-region-point-min)
                  (d12/line-or-region-point-max))
  (move-beginning-of-line 1)
  (yank)
  (message "duplicated"))

(defun d12/delete-line-or-region ()
  "Delete current line or region without putting it to kill-ring. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (delete-region (d12/line-or-region-point-min)
                 (d12/line-or-region-point-max))
  (message "deleted"))

(defun d12/line-or-region-point-min ()
  "Return min point of line or region. When `universal-argument' is called first, returns min point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-beginning)
        (line-beginning-position))
    (point-min)))

(defun d12/line-or-region-point-max ()
  "Return max point of line or region. When `universal-argument' is called first, returns max point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-end)
        (line-beginning-position 2))
    (point-max)))

(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))


;; ---------------
;;; Misc functions
;; ---------------

(defmacro d12|plist-add (list key value)
  `(setq ,list (plist-put ,list ,key ,value)))

(defun d12/toggle-fullscreen ()
  "Cycle thorugh full screen options by rule 'nil -> maximized -> fullboth -> nil'."
  (interactive)
  (let ((x (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (cond ((not x) 'maximized)
                               ((eq x 'maximized) 'fullboth)
                               (t nil)))))

(defun util-count-lines (beg end)
  (let (tmp)
    (if (< end beg) (progn (setq tmp beg) (setq beg end) (setq end tmp)))
    (save-excursion
      (goto-char beg) (setq beg (line-beginning-position))
      (goto-char end) (setq end (line-beginning-position)))
    (count-lines beg end)))

(defun empty-line-suffix () (only-whitespace (current-line-suffix)))

(defun empty-line-prefix () (only-whitespace (current-line-prefix)))

(defun only-whitespace (str) (and (string-match "^[ \r\t]*\$" str) 't))

(defun current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun current-line-full ()
  (buffer-substring (line-beginning-position) (+ 1 (line-end-position))))

(defun current-line-prefix ()
  (buffer-substring (line-beginning-position) (point)))

(defun current-line-suffix () (buffer-substring (point) (line-end-position)))

(defun current-line-number ()
  (let ((linenum (string-to-number (substring (what-line) 5))))
    (message "")
    linenum))

(defun current-number ()
  (save-excursion
    (let (beg)
      (skip-chars-backward "0-9")
      (setq beg (point))
      (skip-chars-forward "0-9")
      (buffer-substring beg (point)))))

(defun d12-fc/format-oos-msg ()
  (interactive)
  (let* ((raw-data (shell-command-to-string "pbpaste")) ; (car kill-ring)
         (msg (substring-no-properties raw-data)))
    (switch-to-buffer "*fc-oos-msg*")
    (end-of-buffer)
    (local-set-key "q" 'kill-this-buffer)
    (string-match "error(\\(.*\\) verifiedHash(\\(.*\\)) clientHash(\\(.*\\)))" msg)
    (insert (format "error: %s\nverified: %s\nclient:   %s\n\n"
                    (match-string 1 msg)
                    (match-string 2 msg)
                    (match-string 3 msg)))))

;;; Misc configs

;; =====================
;;; org mode and friends
;; =====================

;; -------------------
;;; org mode functions
;; -------------------

(defun gtd ()
  (interactive)
  (find-file (concat d12/org-home-path "gtd.org")))

(defun d12/reload-agenda-files ()
  (interactive)
  (let* ((d12/org-ignored-dirs (-flatten
                                (-non-nil
                                 (-map (lambda (dir)
                                         (d12/org-dir-and-subdirs dir))
                                       d12/org-agenda-ignore-dirs))))
         (d12/org-agenda-dirs (-difference (d12/org-dir-and-subdirs "") d12/org-ignored-dirs))
         (d12/org-agenda-files (-flatten (-map (lambda (dir)
                                                 (d12/org-files-in-folder dir))
                                               d12/org-agenda-dirs))))
    (setq org-agenda-files d12/org-agenda-files)))

(defun d12/org-dir-and-subdirs (dir)
  (let ((org-dir (concat d12/org-home-path dir)))
    (when (file-directory-p org-dir)
      (-insert-at 0 org-dir (d12/directory-dirs-r org-dir)))))

(defun d12/org-files-in-folder (folder)
  (directory-files folder t ".*\.org$\\|[0-9]+-[0-9]+-[0-9]+$"))

(defun d12/org-insert-block-template ()
  "Insert block template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("h" . "HTML")
                      ("q" . "QUOTE")
                      ("c" . "CENTER")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "\n#+END_" choice)
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "\n#+END_" choice))))))))))

(defun d12/org-buffer-contains-header? ()
  "Does current buffer contain org header?"
  (interactive)
  (let ((empty (= (point-min)
                  (point-max)))
        (titled (or t (s-contains? "#+TITLE:" (buffer-string) t))))
    (and titled (not empty))))

(defun d12/org-journal-buffer-contains-date-header? ()
  "Does current buffer contain date header?"
  (interactive)
  (buffer-contains-substring? (d12/org-journal-date-header)))

(defun d12/org-guess-title ()
  "Try to guess title for org file.
In case of failure it will use value of d12/org-default-title."
  (let ((bname (buffer-name)))
    (if (s-present? bname)
        (if (s-suffix? ".org" bname)
            (substring bname 0 -4)
          bname)
      d12/org-default-title)))

(defun d12/org-option (width key value)
  "Create an option string for org file."
  (s-append value (s-pad-right width " " (s-concat "#+" key ":"))))

(defun d12/org-sort-current-level ()
  "Sort current level by TODO."
  (interactive)
  (org-sort-entries nil ?o))

(defun d12/org-sort-upper-level ()
  "Go to upper level and sort it by TODO."
  (interactive)
  (progn (outline-up-heading 1)
         (d12/org-sort-current-level)))

(defun d12/org-create-new-org-file (name)
  (interactive "sEnter the name of new file: ")

  (let ((existing-files (d12/directory-dirs d12/org-home-path))
        (new-file-dir (s-concat d12/org-home-path name) ))
    (if (-contains? existing-files new-file-dir)
        (message "Sorry, but there is already file named '%s'." name)
      (progn (dired-create-directory new-file-dir)
             (dired-create-directory (s-concat new-file-dir "exports"))
             (dired-create-directory (s-concat new-file-dir "assets"))
             (with-temp-buffer (write-file (s-concat new-file-dir (s-append ".org" name))))))))

(defun d12/org-insert-date (&optional days)
  "Insert timestamp formated by value of d12/org-date-format.
If optional argument DAYS is non-nil and number or marker, then
it will be added to current date."
  (interactive "P")
  (if (or (eq days nil)
          (not (number-or-marker-p days)))
      (insert (format-time-string d12/org-date-format))
    (insert (format-time-string d12/org-date-format (time-add (current-time) (days-to-time days))))))

(defun d12/org-insert-time ()
  "Insert timestamp formated by value of d12/org-time-format"
  (interactive)
  (insert (format-time-string d12/org-time-format)))

(defun d12/org-insert-full-date ()
  "Insert date and timestamp. Uses 'd12/org-insert-date
  and 'd12/org-insert-time."
  (interactive)
  (insert (format-time-string (concat d12/org-date-format " " d12/org-time-format))))

(defun org-journal-visit-entry ()
  (interactive)
  (setq current-prefix-arg '(t))
  (call-interactively 'org-journal-new-entry))

(defun d12/org-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice d12/delete-line-or-region (after fix-cookies activate)
  (d12/org-update-parent-cookie))

(defadvice d12/duplicate-line-or-region (after fix-cookies activate)
  (d12/org-update-parent-cookie))

(defadvice d12/cut-line-or-region (after fix-cookies activate)
  (d12/org-update-parent-cookie))

(defun d12/org-publish-org-sitemap (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :sitemap-root)))
         (base-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (localdir (file-name-directory dir))
         (indent-str (make-string 2 ?\ ))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filepath (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-title (or (plist-get project-plist :sitemap-title)
                            (concat "Sitemap for project " (car project))))
         (sitemap-style (or (plist-get project-plist :sitemap-style)
                            'tree))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filepath))
         (ifn (file-name-nondirectory sitemap-filepath))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file sitemap-filepath))))
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link (file-relative-name file dir))
              (oldlocal localdir))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ;; sitemap shouldn't list itself
          (unless (equal (file-truename (concat base-dir (or sitemap-filename "sitemap.org")))
                         (file-truename file))
            (if (eq sitemap-style 'list)
                (message "Generating list-style sitemap for %s" sitemap-title)
              (message "Generating tree-style sitemap for %s" sitemap-title)
              (setq localdir (concat (file-name-as-directory dir)
                                     (file-name-directory link)))
              (unless (string= localdir oldlocal)
                (if (string= localdir dir)
                    (setq indent-str (make-string 2 ?\ ))
                  (let ((subdirs
                         (split-string
                          (directory-file-name
                           (file-name-directory
                            (file-relative-name localdir dir))) "/"))
                        (subdir "")
                        (old-subdirs (split-string
                                      (file-relative-name oldlocal dir) "/")))
                    (setq indent-str (make-string 2 ?\ ))
                    (while (string= (car old-subdirs) (car subdirs))
                      (setq indent-str (concat indent-str (make-string 2 ?\ )))
                      (pop old-subdirs)
                      (pop subdirs))
                    (dolist (d subdirs)
                      (setq subdir (concat subdir d "/"))
                      (insert (concat indent-str " + " d "\n"))
                      (setq indent-str (make-string
                                        (+ (length indent-str) 2) ?\ )))))))
            ;; This is common to 'flat and 'tree
            (let ((entry
                   (org-publish-format-file-entry
                    org-publish-sitemap-file-entry-format file project-plist))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              (cond ((string-match-p regexp entry)
                     (string-match regexp entry)
                     (insert (concat indent-str " + " (match-string 1 entry)
                                     "[[file:" link "]["
                                     (match-string 2 entry)
                                     "]]" (match-string 3 entry) "\n")))
                    (t
                     (insert (concat indent-str " + [[file:" link "]["
                                     entry
                                     "]]\n"))))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

;; ------------------------
;;; org mode initialization
;; ------------------------

(defun d12frosted/post-init-org ()
  "Initialize org package."
  (use-package org
    :defer t
    :init
    (evil-leader/set-key
      ".a" 'org-agenda
      ".it" 'd12/org-insert-time
      ".id" 'd12/org-insert-date
      ".iD" 'd12/org-insert-full-date
      ".c" 'org-clock-out
      ".C" 'org-clock-in-last
      ".l" 'org-store-link)
    :config
    (evil-leader/set-key-for-mode 'org-mode
      "ms" 'd12/org-sort-current-level
      "mS" 'd12/org-sort-upper-level
      "m#" 'd12/org-insert-block-template)
    (require 's)

    (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
      (let ((rlt ad-return-value)
            (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\)")
            (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\)")
            old-flag
            b e)
        (when ad-return-value
          (save-excursion
            (setq old-flag case-fold-search)
            (setq case-fold-search t)
            (setq b (re-search-backward begin-regexp nil t))
            (if b (setq e (re-search-forward end-regexp nil t)))
            (setq case-fold-search old-flag))
          (if (and b e (< (point) e)) (setq rlt nil)))
        (setq ad-return-value rlt)))

    (defun org-clock-get-clock-string ()
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
  (let ((clocked-time (org-clock-get-clocked-time)))
    (if org-clock-effort
        (let* ((effort-in-minutes
                (org-duration-string-to-minutes org-clock-effort))
               (work-done-str
                (org-propertize
                 (org-minutes-to-clocksum-string clocked-time)
                 'face (if (and org-clock-task-overrun (not org-clock-task-overrun-text))
                           'org-mode-line-clock-overrun 'org-mode-line-clock)))
               (effort-str (org-minutes-to-clocksum-string effort-in-minutes))
               (clockstr (org-propertize (concat  " [%s/" effort-str "] ")
                                         'face 'org-mode-line-clock)))
          (format clockstr work-done-str))
      (org-propertize (concat "[" (org-minutes-to-clocksum-string clocked-time) "]")
                      'face 'org-mode-line-clock))))

    (setq org-todo-keywords
          '((sequence
             ;; The item is ready to be done at the earliest opportunity or
             ;; at the date (and maybe time) indicated in the SCHEDULED tag.
             ;; Some tasks are given a DEADLINE date which is useful for
             ;; scheduling the tasks during my daily planning.
             "TODO(t)"

             ;; I should use this tag when I start on a task, but if I clock
             ;; in to a TODO item, I don't really need this task.
             "STARTED(s)"

             ;; I did some work on this task but I am waiting for a response.
             ;; If I use this task I schedule the task into the future as a
             ;; reminder to follow up with some notes in the body of the task.
             "WAITING(w)"

             ;; Used to tag an activity that can only be done at the specified
             ;; time and date, instead of tasks that can be
             ;; completed at any time.
             "APPT(a)"

             "|"

             ;; The task is completed.
             "DONE(d)"

             ;; I decided not to do this task but have left the task on file
             ;; with this status.
             "CANCELLED(c)"

             ;; Used to identify a task that will not be activated just yet.
             ;; The reason will be included in the task notes.
             "DELAYED(l)"))

          org-agenda-window-setup 'current-window
          org-src-fontify-natively t
          org-directory d12/org-home-path
          org-agenda-inhibit-startup nil
          org-mobile-inbox-for-pull (concat d12/org-home-path "mobile.org")
          org-mobile-force-id-on-agenda-items nil
          org-mobile-directory "~/Dropbox/Apps/MobileOrg/")

    ;; (require 'ox-publish)
    ;; (setq org-html-htmlize-output-type 'inline-css
    ;;       org-html-validation-link nil
    ;;       org-publish-project-alist d12-blog/projects)

    (d12/reload-agenda-files)
    (d12|rename-modeline "org" org-mode "本")
    )
  )

(defun d12frosted/init-org-journal ()
  "Initialize org-journal package"
  (use-package org-journal
    :ensure t
    :mode (".*/[0-9]*-[0-9]*-[0-9]*$" . org-journal-mode)
    :init
    (evil-leader/set-key
      ".j" 'calendar
      ".n" 'org-journal-new-entry
      ".v" 'org-journal-visit-entry)
    (d12|rename-modeline "org-journal" org-journal-mode "日記")
    :config
    (global-unset-key (kbd "C-c C-j"))
    (setq org-journal-dir (concat d12/org-home-path "journal/")
          org-journal-date-format d12/org-date-format
          org-journal-time-format "%R\n"
          org-journal-file-format "%Y-%m-%d"
          org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
          org-journal-hide-entries-p nil))
  )

;; =====================
;;; Csharp and Omnisharp
;; =====================

(defun csharp-hs-forward-sexp (&optional arg)
  "Stolen from emacswiki"
  (message "csharp-hs-forward-sexp, (arg %d) (point %d)..."
           (if (numberp arg) arg -1)
           (point))

  (let ((nestlevel 0)
        (mark1 (point))
        (done nil))

    (if (and arg (< arg 0))
        (message "negative arg (%d) is not supported..." arg)

      ;; else, we have a positive argument, hence move forward.
      ;; simple case is just move forward one brace
      (if (looking-at "{")
          (forward-sexp arg)

        ;; The more complex case is dealing with a "region/endregion" block.
        ;; We have to deal with nested regions!
        (and
         (while (not done)
           (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                              (point-max) 'move)
           (cond

            ;; do nothing if at end of buffer
            ((eobp))

            ((and
              (match-beginning 1)
              ;; if the match is longer than 6 chars, we know it is "endregion"
              (if (> (- (match-end 1) (match-beginning 1)) 6)
                  (setq nestlevel (1- nestlevel))
                (setq nestlevel (1+ nestlevel))))))

           (setq done (not (and (> nestlevel 0) (not (eobp))))))

         (if (= nest 0)
             (goto-char (match-end 2))))))))

(defun d12/omnisharp-go-to-definition-at-center ()
  (interactive)
  (progn
    (omnisharp-go-to-definition)
    (recenter)))

(defun d12/omnisharp-comment-to-doc ()
  "Convert regular comment at point int to documentation comment."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward-regexp "\\([ \t]+\\)//\\(.*\\)" nil t)
      (replace-match (concat (match-string 1)
                             "/// <summary>\n"
                             (match-string 1)
                             "///"
                             (match-string 2)
                             "\n"
                             (match-string 1)
                             "/// </summary>") t nil))))


(defun d12frosted/post-init-omnisharp ()
  "Initialize omnisharp package."
  (use-package omnisharp
    :defer t
    :init
    (add-hook 'csharp-mode-hook 'hs-minor-mode)
    (add-hook 'csharp-mode-hook 'hide-ifdef-mode)
    (add-hook 'csharp-mode-hook 'flycheck-mode)
    :config
    (require 'company)
    (spacemacs|diminish omnisharp-mode " ♯" " #")
    (spacemacs|hide-lighter hs-minor-mode)
    (spacemacs|hide-lighter hide-ifdef-mode)

    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "mc" "compile")
    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "mf" "file")
    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "mp" "projectile")
    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "mg" "navigation")
    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "mh" "documentation")
    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "mr" "refactoring")
    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "ms" "server")
    ;; (spacemacs/declare-prefix-for-mode 'omnisharp-mode "mt" "tests")

    (defun omnisharp--get-omnisharp-server-executable-command
        (solution-file-path &optional server-exe-file-path)
      (let* ((server-exe-file-path-arg (expand-file-name
                                        (if (eq nil server-exe-file-path)
                                            omnisharp-server-executable-path
                                          server-exe-file-path)))
             (solution-file-path-arg (expand-file-name solution-file-path))
             (args (list server-exe-file-path-arg
                         "-s"
                         solution-file-path-arg)))
        (cond
         ((or (equal system-type 'cygwin)                    ; No mono needed on cygwin
              (not (s-suffix-p ".exe" server-exe-file-path)) ; No mono needed on roslyn
              (equal system-type 'windows-nt))
          args)
         (t                                                  ; some kind of unix: linux or osx
          (cons "mono" args)))))

    (bind-keys
     :map csharp-mode-map
     ;; Some usefull shotcuts
     ("M-." . d12/omnisharp-go-to-definition-at-center)
     ("M-," . pop-tag-mark)
     ("C-c <" . hs-hide-block)
     ("C-c >" . hs-show-block)
     ("C-c c <" . hide-ifdef-block)
     ("C-c c >" . show-ifdef-block))

    (unless (assoc 'csharp-mode hs-special-modes-alist)
      (push '(csharp-mode
              ;; regexp for start block
              "\\([ \\t]*#[ \\t]*region\\b\\)\\|{"

              ;; regexp for end block
              "\\([ \\t]*#[ \\t]*endregion\\b\\)\\|}"

              "/[*/]"                                 ; regexp for comment start
              csharp-hs-forward-sexp                  ; hs-forward-sexp-func
              hs-c-like-adjust-block-beginning        ; c-like adjust (1 char)
              )
            hs-special-modes-alist)))
  )

;; ======================
;;; Elfeed configurations
;; ======================

(defun d12frosted/init-elfeed ()
  "Initialize elfeed package."
  (use-package elfeed
    :defer 1
    :init
    (evil-leader/set-key "ae" 'elfeed)
    :config
    ;; todo - move to configurable variable
    (setq elfeed-feeds '(("http://www.reddit.com/r/haskelltil/.rss" haskell reddit)
                         ("http://www.reddit.com/r/haskell/.rss" haskell reddit)
                         ("http://www.reddit.com/r/orgmode/.rss" emacs org-mode reddit)
                         ("http://planet.haskell.org/rss20.xml" haskell)
                         ("http://www.reddit.com/r/emacs/.rss" emacs reddit)
                         ("http://nullprogram.com/feed/" emacs)
                         ("http://endlessparentheses.com/atom.xml" emacs)))))

(defun d12frosted/init-elfeed-web ()
  "Initialize elfeed-web package."
  (use-package elfeed-web
    :defer t))

;; For each package, define a function d12frosted/init-<package-d12frosted>
;;
;; (defun d12frosted/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
