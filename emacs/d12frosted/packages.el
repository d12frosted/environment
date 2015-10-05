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

(setq d12frosted-packages
      '(
        org
        org-journal
        omnisharp
        shm
        ))

(setq d12frosted-excluded-packages '())

;;; Misc configs

;; =============================================================================
;; org mode
;; =============================================================================

(defun d12frosted/post-init-org ()
  "Initialize org package."
  (use-package org
    :defer t
    :init
    (evil-leader/set-key
      ".a" 'org-agenda
      ".c" 'org-clock-out
      ".C" 'org-clock-in-last
      ".l" 'org-store-link)
    :config
    (evil-leader/set-key-for-mode 'org-mode
      "m1" 'd12/org-sort-current-level
      "m!" 'd12/org-sort-upper-level
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
          org-mobile-directory "~/Dropbox/Apps/MobileOrg/"

          org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

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
          org-journal-date-format d12/date-format
          org-journal-time-format "%R\n"
          org-journal-file-format "%Y-%m-%d"
          org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
          org-journal-hide-entries-p nil))
  )

;; =============================================================================
;;; Csharp and Omnisharp
;; =============================================================================

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
            hs-special-modes-alist))))

(defun d12frosted/post-init-shm ()
  (use-package shm
    :defer t
    :init
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    (defadvice spacemacs/init-haskell-mode (after d12//init-haskell-mode activate)
      (setq-local global-hl-line-mode nil))
    :config
    (progn
      (when (require 'shm-case-split nil 'noerror)
        (define-key shm-map (kbd "C-c S") 'shm/case-split)
        (define-key shm-map (kbd "C-c C-s") 'shm/do-case-split)))))
