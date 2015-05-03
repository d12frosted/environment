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

;;; plist functions
;; =================

(defun d12/mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.
A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.
If there are multiple properties with the same keyword, only the first property
and its values is returned.
Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun d12/mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.
If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))
;;; Key binding helpers
;; =====================

(defun d12/create-key-binding-form (props func)
  "Helper which returns a from to bind FUNC to a key according to PROPS.
  Supported properties:
  `:bind-global KEY-NAME'
      One or several key sequence strings to be set with `global-set-key'.
  `:bind-local CONS CELL'
      One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'. "
  (let ((bind-global (d12/mplist-get props :bind-global))
        (bind-local (d12/mplist-get props :bind-local)))
    `((unless (null ',bind-global)
        (dolist (key ',bind-global)
          (global-set-key (kbd key) ',func)))
      (unless (null ',bind-local)
        (dolist (val ',bind-local)
          (define-key (eval (car val)) (kbd (cdr val)) ',func))))))
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
