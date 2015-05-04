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

;;; Navigation
;; ============

(defun d12/goto-line-and-center ()
  (interactive)
  (call-interactively 'goto-line)
  (call-interactively 'recenter-top-bottom))

;; Thanks to Sylvain Benner
(defun d12/smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

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

;; Thanks to Sylvain Benner
(defmacro d12|add-toggle (name &rest props)
  "Add a toggle with NAME symbol.
  Available PROPS:
  `:status EXPRESSION'
      The EXPRESSION to evaluate to get the current status of the toggle.
  `:if EXPRESSION'
      If this EXPRESSION evaluate to nil then no attempt to update the toggle
      status will be performed.
  `:on BODY'
      Evaluate BODY when the toggle is switched on.
  `:off BODY'
      Evaluate BODY when the toggle is switched off.
  `:documentation STRING'
      STRING describes what the toggle does.
  `:bind-global KEY-NAME'
      Bind KEY-NAME to toggle globally.
  `:bind-local KEY-NAME KEY-MAP
      Bind KEY-NAME to toggle in KEY-MAP"
  (let* ((wrapper-func (intern (format "d12/toggle-%s"
                                       (symbol-name name))))
         (status (plist-get props :status))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (d12/mplist-get props :on))
         (off-body (d12/mplist-get props :off))
         (bindkeys (d12/create-key-binding-form props wrapper-func)))
    `(progn
       ;; toggle function
       (defun ,wrapper-func ()
         ,(format "Toggle %s on and off." (symbol-name name))
         (interactive)
         ;; we evaluate condition and status only if they are a list or
         ;; a bound symbol
         (if (or (null ',condition)
                   (and (or (and (symbolp ',condition) (boundp ',condition))
                            (listp ',condition))
                        ,condition))
             (if (and (or (and (symbolp ',status) (boundp ',status))
                          (listp ',status))
                      ,status) (progn ,@off-body) ,@on-body)
           (message "This toggle is not supported.")))
       ,@bindkeys)))
