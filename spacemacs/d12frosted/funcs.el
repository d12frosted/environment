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

;; =============================================================================
;; General helper functions
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Files and directories
;; -----------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------------
;; Navigation
;; -----------------------------------------------------------------------------

(defun d12/goto-line-and-center ()
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

;; -----------------------------------------------------------------------------
;; Mode renaming and diminishing
;; -----------------------------------------------------------------------------

(defmacro d12|rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after d12|rename-modeline-hack activate)
        (setq mode-name ,new-name))))

(defmacro d12|diminish (mode dim)
  "Diminish MODE name in mode line to DIM."
  `(eval-after-load 'diminish '(diminish ',mode ,dim)))

;; -----------------------------------------------------------------------------
;; Text manipulations
;; -----------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------------
;; Misc functions
;; -----------------------------------------------------------------------------

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
