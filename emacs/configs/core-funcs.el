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

(defun d12/get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; from magnars
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;;; Buffers
;; =========

(defun buffer-contains-substring? (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

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

;; thanks to https://github.com/kai2nenobu/guide-key/wiki
(defmacro d12|define-prefix-global (key-name prefix-name)
  "Define prefix command for KEY-NAME globally.
   Uses 'd12/guide-prefix to build the name."
  (let ((name (intern (concat d12/guide-prefix (symbol-name prefix-name)))))
    `(progn (define-prefix-command ',name)
            (bind-key ,key-name ',name))))

(defmacro d12|define-prefix-local (key-name prefix-name key-map)
  "Define prefix command for KEY-NAME fro KEY-MAP.
   Uses 'd12/guide-prefix to build the name."
  (let ((name (intern (concat d12/guide-prefix (symbol-name prefix-name)))))
    `(progn (define-prefix-command ',name)
            (bind-key ,key-name ',name ,key-map))))

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

(defun util-forward-word ()
  "Move one word forward. Leave the pointer at start of word
  instead of emacs default end of word. Treat _ as part of word"
  (interactive)
  (let ((start (point)) boundary at-boundary jump)
    (setq at-boundary (eolp))
    (setq boundary (line-end-position))
    (forward-char 1)
    (backward-word 1)
    (forward-word 2)
    (backward-word 1)
    (backward-char 1)
    (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\.")) (forward-char 1)
           (util-forward-word))
          (t (forward-char 1)))
    (if (and (not at-boundary) (> (point) boundary)) (goto-char boundary))
    (setq jump (util-count-lines start (point)))
    (if (> jump 0) (progn (forward-line (- 1 jump)) (back-to-indentation)))))

(defun util-backward-word ()
  "Move one word backward. Leave the pointer at start of word
  Treat _ as part of word."
  (interactive)
  (let ((start (point)) boundary at-boundary jump)
    (setq at-boundary (empty-line-prefix))
    (setq boundary (line-beginning-position))
    (backward-word 1)
    (backward-char 1)
    (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\."))
           (util-backward-word))
          (t (forward-char 1)))
    (if (and (not at-boundary) (< (point) boundary))
        (progn (goto-char boundary) (back-to-indentation)))
    (setq jump (util-count-lines start (point)))
    (if (> jump 0) (progn (forward-line (- jump 1)) (end-of-line)))))

;;; Mode renaming and diminishing
;; ===============================

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

;;; Various stuff
;; ===============

(defun spacemacs//set-dotted-directory ()
  "Set the face of diretories for `.' and `..'"
  (set-face-attribute 'helm-ff-dotted-directory
                      nil
                      :foreground nil
                      :background nil
                      :inherit 'helm-ff-directory))

(defun d12/toggle-fullscreen ()
  "Cycle thorugh full screen options by rule 'nil -> maximized -> fullboth -> nil'."
  (interactive)
  (let ((x (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (cond ((not x) 'maximized)
                               ((eq x 'maximized) 'fullboth)
                               (t nil)))))

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

(defun d12/disable-hl-line-mode ()
  "Disable `global-hl-line-mode' locally."
  (setq-local global-hl-line-mode nil))

;;; Text manipulations
;; --------------------

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
