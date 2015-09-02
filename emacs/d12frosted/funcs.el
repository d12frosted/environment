;;; funcs.el --- d12frosted Layer funcs File for Spacemacs
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

(defun gtd ()
  (interactive)
  (find-file (concat d12/org-home-path "gtd.org")))

(defun d12/recursive-load-dir-settings (currentfile)
  (let ((lds-dir (locate-dominating-file currentfile d12/dir-settings-file)))
    (when lds-dir
      (progn
        (load-file (concat lds-dir d12/dir-settings-file))
        (d12/recursive-load-dir-settings (file-truename(concat lds-dir "..")))))))

(defun d12/load-dir-settings()
  (interactive)
  (when buffer-file-name
    (d12/recursive-load-dir-settings buffer-file-name)))

;; =============================================================================
;; Files and directories
;; =============================================================================

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

;; =============================================================================
;; Navigation
;; =============================================================================

(defun d12/goto-line-and-center ()
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

;; =============================================================================
;; Mode renaming and diminishing
;; =============================================================================

(defmacro d12|rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after d12|rename-modeline-hack activate)
        (setq mode-name ,new-name))))

(defmacro d12|diminish (mode dim)
  "Diminish MODE name in mode line to DIM."
  `(eval-after-load 'diminish '(diminish ',mode ,dim)))

;; =============================================================================
;; Text manipulations
;; =============================================================================

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

;; =============================================================================
;; comment-or-uncomment-sexp
;; http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html?source=rss
;; =============================================================================

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

;; =============================================================================
;; mu4e functions
;; =============================================================================

(defun d12-mu4e/set-account-vars (account)
  "Set the variables for ACCOUNT from 'mu4e/accounts-alist."
  (setq d12-mu4e/current-account account)
  (let* ((account-vars (cdr (assoc account d12-mu4e/accounts-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "Couln't find variables for <%s>" account))))

(defun d12-mu4e/set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir
                     (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (helm-comp-read
             "Compose with account:"
             (mapcar (lambda (var) (car var)) d12-mu4e/accounts-alist)))))
    (d12-mu4e/set-account-vars account)))

(defun d12-mu4e/msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))

(defun d12-mu4e/mail-account-reset ()
  "Reset mail account info to first."
  (d12-mu4e/set-account-vars d12-mu4e/default-account))

;; -----------------------------------------------------------------------------
;; *-folder functions
;; -----------------------------------------------------------------------------

(defun d12-mu4e/get-folder (type msg)
  "Returns the folder of TYPE based on msg.
   If MSG is nil then returns the folder of
   TYPE based on 'mu4e/current-account."
  (let* ((account (if msg
                      (d12-mu4e/get-account-from-maildir
                       (mu4e-message-field msg :maildir))
                    d12-mu4e/current-account)))
    (d12-mu4e/get-prop-for-account-in-alist account
                                            type
                                            d12-mu4e/folders-alist)))

(defun d12-mu4e/trash-folder-fn (msg)
  "Returns trash folder for MSG."
  (d12-mu4e/get-folder 'mu4e-trash-folder msg))

(defun d12-mu4e/refile-folder-fn (msg)
  "Returns refile folder for MSG."
  (d12-mu4e/get-folder 'mu4e-refile-folder msg))

(defun d12-mu4e/drafts-folder-fn (msg)
  "Returns drafts folder for MSG."
  (d12-mu4e/get-folder 'mu4e-drafts-folder msg))

(defun d12-mu4e/sent-folder-fn (msg)
  "Returns sent folder for MSG."
  (d12-mu4e/get-folder 'mu4e-sent-folder msg))

;; -----------------------------------------------------------------------------
;; Helper functions
;; -----------------------------------------------------------------------------

(defun d12-mu4e/get-account-from-maildir (maildir)
  "Return account name for maildir.
   For example, (mu4e/get-account-from-maildir \"/some-name/Trash\")
   returns \"some-name\"."
  (string-match "/\\(.*?\\)/" maildir)
  (match-string 1 maildir))

(defun d12-mu4e/get-prop-for-account-in-alist (account prop alist)
  (let* ((props (cdr (assoc account alist)))
         (value (car (cdr (assoc prop props)))))
    (if value
        value
      (error "Couldn't find '%s' property for '%s' in %s"
             prop
             account
             alist))))

;; =============================================================================
;; Misc functions
;; =============================================================================

(defun d12-mu4e/load-private-configs ()
  "Load private mu4e configurations from `d12-mu4e/private-config-path'")

(defun d12/insert-date (&optional days)
  "Insert timestamp formated by value of `d12/date-format'.
If optional argument DAYS is non-nil and number or marker, then
it will be added to current date."
  (interactive "P")
  (if (or (eq days nil)
          (not (number-or-marker-p days)))
      (insert (format-time-string d12/date-format))
    (insert (format-time-string d12/date-format (time-add (current-time) (days-to-time days))))))

(defun d12/insert-time ()
  "Insert timestamp formated by value of `d12/time-format'"
  (interactive)
  (insert (format-time-string d12/time-format)))

(defun d12/insert-full-date ()
  "Insert date and timestamp. Uses 'd12/insert-date
  and 'd12/insert-time."
  (interactive)
  (insert (format-time-string (concat d12/date-format " " d12/time-format))))

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

;;; funcs.el ends here
