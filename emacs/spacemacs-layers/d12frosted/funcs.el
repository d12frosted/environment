;;; funcs.el --- d12frosted Layer funcs File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; general functions

(defun d12frosted/concat-path (dir f)
  "Append F to DIR with respect of trailing slash in DIR."
  (if (s-ends-with? "/" dir)
      (s-append f dir)
    (s-concat dir "/" f)))

(defun d12frosted/directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (-filter (lambda (f) (file-directory-p f))
           (-map (lambda (f) (d12frosted/concat-path dir f))
                 (-filter (lambda (f) (not (-contains? '("." "..") f))) (directory-files dir))))
  )

(defun d12frosted/directory-dirs-r (dir)
  "Find all directories in DIR (recursive)."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (d12frosted/directory-dirs-r file))
                               dirs))))))
    dirs))

(defun rename-current-file (new-name)
  "Change the name of current file and buffer to NAME."
  (interactive "sEnter new name for this file: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun d12frosted/invalidate-cache ()
  "Invalidate projectile and recentf cache."
  (interactive)
  (progn (projectile-invalidate-cache nil)
         (recentf-cleanup)))

;;; text manipulations

(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun copy-line-or-region ()
  "Copy current line (with newline character) or region. When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (line-or-region-point-min)
                  (line-or-region-point-max))
  (message "copied"))

(defun cut-line-or-region ()
  "Cut current line or region. When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-region (line-or-region-point-min)
               (line-or-region-point-max))
  (message "cut"))

(defun duplicate-line-or-region ()
  "Duplicates current line or region. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (line-or-region-point-min)
                  (line-or-region-point-max))
  (move-beginning-of-line 1)
  (yank)
  (message "duplicated"))

(defun delete-line-or-region ()
  "Delete current line or region without putting it to kill-ring. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (delete-region (line-or-region-point-min)
                 (line-or-region-point-max))
  (message "deleted"))

(defun line-or-region-point-min ()
  "Return min point of line or region. When `universal-argument' is called first, returns min point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-beginning)
        (line-beginning-position))
    (point-min)))

(defun line-or-region-point-max ()
  "Return max point of line or region. When `universal-argument' is called first, returns max point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-end)
        (line-beginning-position 2))
    (point-max)))
