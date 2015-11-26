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
;;
;;; Code:

;;; Getting things done

(defun gtd ()
  "Open gtd.org file in `d12/org-home-path'."
  (interactive)
  (find-file (concat d12/org-home-path "gtd.org")))

(defun d12/helm-spotlight ()
  (interactive)
  (helm :buffer "*helm: spotlight*"
        :sources '(helm-source-mac-spotlight)))

(defun d12/helm-gtd ()
  "Org files discovery with helm interface."
  (interactive)
  (helm :buffer "*helm: gtd*"
        :sources `(,(d12/helm-gtd/source))))

(defun d12/helm-gtd/source ()
  "Construct helm source for org files in `d12/org-home-path'."
  `((name . "Files")
    (candidates . ,(sort (d12/helm-gtd/get-files-list) 'string<))
    (candidate-number-limit)
    (action . (("Open file" . d12/helm-gtd/open-org-file)))))

(defun d12/helm-gtd/get-files-list ()
  "Get the list of org files in `d12/org-home-path'."
  (directory-files d12/org-home-path nil ".*\.org$"))

(defun d12/helm-gtd/open-org-file (candidate)
  "Open file in `d12/org-home-path'."
  (find-file (concat d12/org-home-path candidate)))

(defun d12/helm-configs ()
  "Config files discovery with helm interface."
  (interactive)
  (helm :buffer "*helm: configs*"
        :sources `(,(d12/helm-configs/source))))

(defun d12/helm-configs/source ()
  "Construct helm source for some configuration files."
  `((name . "Files")
    (candidates . (,(concat d12/spacemacs-dir "init.el")
                   ,(concat d12/emacs-private-path "private.el")
                   ,(concat d12/fish-public-path "config.fish")
                   ,(concat d12/fish-private-path "preconfig.fish")
                   ,(concat d12/fish-private-path "postconfig.fish")))
    (candidate-number-limit)
    (action . (("Open file" . find-file)))))

;;; Custom settings loader

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

;;; Files and directories

(defun d12/directory-dirs (directory)
  "Return a list of names of directories in DIRECTORY excluding
  '.' and '..'."
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
  "Return a list of names of directories in DIRECTORY and all
  it's subdirectories excluding '.' and '..'."
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

;;; Navigation

(defun d12/goto-line-and-center ()
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

;;; Mode renaming and diminishing

(defmacro d12|rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after d12|rename-modeline-hack activate)
        (setq mode-name ,new-name))))

(defmacro d12|diminish (mode dim)
  "Diminish MODE name in mode line to DIM."
  `(eval-after-load 'diminish '(diminish ',mode ,dim)))

;;; Text manipulations

(defun d12/copy-line-or-region (&optional copy-func)
  "Copy current line (with newline character) or region. When
`universal-argument' is called first, copy whole buffer (but
respect `narrow-to-region').

When `copy-func' is provided, it is used to copy line or region
instead of `kill-ring-save'"
  (interactive)
  (if (fboundp copy-func)
      (d12//funcall-on-line-or-region copy-func)
    (d12//funcall-on-line-or-region 'copy-region-as-kill))
  (message "copied"))

(defun d12/kill-line-or-region (&optional kill-func)
  "Cut current line or region. When `universal-argument' is
called first, cut whole buffer (but respect `narrow-to-region').

When `kill-func' is provided, it is used to copy line or region
instead of `kill-region'"
  (interactive)
  (if (fboundp kill-func)
      (d12//funcall-on-line-or-region kill-func)
    (d12//funcall-on-line-or-region 'kill-region))
  (message "killed"))

(defun d12/delete-line-or-region (&optional delete-func)
  "Delete current line or region without putting it to kill-ring.
When `universal-argument' is called first, delete whole
buffer (but respect `narrow-to-region').

When `kill-func' is provided, it is used to copy line or region
instead of `kill-region'"
  (interactive)
  (if (fboundp delete-func)
      (d12//funcall-on-line-or-region delete-func)
    (d12//funcall-on-line-or-region 'delete-region))
  (message "removed"))

(defun d12//funcall-on-line-or-region (func)
  "Call function `f' on current line or region."
  (if current-prefix-arg
      (funcall func (point-min) (point-max))
    (if (use-region-p)
        (funcall func (region-beginning) (region-end) t)
      (funcall func (line-beginning-position) (line-beginning-position 2)))))

;;; comment-or-uncomment-sexp
;; http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html?source=rss

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
  "Comment the sexp at point and move past it. If already
inside (or before) a comment, uncomment instead. With a prefix
argument N, (un)comment that many sexps."
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

;;; Misc functions

(defun d12/insert-date (&optional days)
  "Insert timestamp formated by value of `d12/date-format'. If
optional argument DAYS is non-nil and number or marker, then it
will be added to current date."
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

;;; funcs.el ends here
