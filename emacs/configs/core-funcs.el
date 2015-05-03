;;; core-funcs.el --- funcs file of core configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Files and Directories
;; =======================

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

;;; Various stuff
;; ===============

(defun d12/toggle-fullscreen ()
  "Cycle thorugh full screen options by rule 'nil -> maximized -> fullboth -> nil'."
  (interactive)
  (let ((x (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (cond ((not x) 'maximized)
			       ((eq x 'maximized) 'fullboth)
			       (t nil)))))

(defmacro d12|diminish (mode dim)
  "Diminish MODE name in mode line to DIM."
  `(eval-after-load 'diminish '(diminish ',mode ,dim)))
