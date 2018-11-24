;;; emacs/buffer/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Nov 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defvar +buffer-real-p-functions
  '(+buffer-dired-p)
  "A list of predicate functions run to determine if a buffer is
real, unlike `+buffer-unreal-p-functions'. They are passed one
argument: the buffer to be tested.

Should any of its function returns non-nil, the rest of the
functions are ignored and the buffer is considered real.

See `+buffer-real-p' for more information.")

;;;###autoload
(defvar +buffer-unreal-p-functions
  '(minibufferp +buffer-special-p +buffer-non-file-visiting-p)
  "A list of predicate functions run to determine if a buffer is *not* real,
unlike `+buffer-real-p-functions'. They are passed one argument:
the buffer to be tested.

Should any of these functions return non-nil, the rest of the
functions are ignored and the buffer is considered unreal.

See `+buffer-real-p' for more information.")

;;;###autoload
(defvar-local +buffer-real-p nil
  "If non-nil, this buffer should be considered real no matter
what. See `+buffer-real-p' for more information.")

;;;###autoload
(defvar +buffer-fallback-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers
exist (will create it if it doesn't exist).")

;;;###autoload
(defvar +buffer-cleanup-session-hook ()
  "A list of hooks run when `+buffer/cleanup-session' is run,
meant to clean up leftover buffers and processes.")

;;
;; Functions

;;;###autoload
(defun +buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter.
Returns nil if BUF should be skipped over by functions like
`next-buffer' and `other-buffer'."
  (or (+buffer-real-p buf)
      (eq buf (+buffer-fallback))))

;;;###autoload
(defun +buffer-fallback ()
  "Returns the fallback buffer, creating it if necessary. By
default this is the scratch buffer. See `+buffer-fallback-name'
to change this."
  (get-buffer-create +buffer-fallback-name))

;;;###autoload
(defalias '+buffer-list #'buffer-list)

;;;###autoload
(defun +buffer-dired-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun +buffer-special-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

;;;###autoload
(defun +buffer-temp-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun +buffer-non-file-visiting-p (buf)
  "Returns non-nil if BUF does not have a value for
`buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun +buffer-real-list (&optional buffer-list)
  "Return a list of buffers that satify `+buffer-real-p'."
  (cl-remove-if-not #'+buffer-real-p (or buffer-list (+buffer-list))))

;;;###autoload
(defun +buffer-real-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.

A real buffer is a useful buffer; a first class citizen. Real
ones should get special treatment, because we will be spending
most of our time in them. Unreal ones should be low-profile and
easy to cast aside, so we can focus on real ones.

The exact criteria for a real buffer is:

  1. A non-nil value for the buffer-local value of the
     `+buffer-real-p' variable OR
  2. Any function in `+buffer-real-p-functions' returns
     non-nil OR
  3. None of the functions in `+buffer-unreal-p-functions'
     must return non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is
tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let* ((buf (get-buffer buffer-or-name)))
    (and (not (+buffer-temp-p buf))
         (or (buffer-local-value '+buffer-real-p buf)
             (run-hook-with-args-until-success '+buffer-real-p-functions buf)
             (not (run-hook-with-args-until-success '+buffer-unreal-p-functions buf))))))

;;;###autoload
(defun +buffer-unreal-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an 'unreal' buffer.

See `+buffer-real-p' for details on what that means."
  (not (+buffer-real-p buffer-or-name)))

;;;###autoload
(defun +buffer-in-mode-list (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (nucleus-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (+buffer-list)))))

;;;###autoload
(defun +buffer-visible-list (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (cl-loop for buf in (or buffer-list (+buffer-list))
           when (get-buffer-window buf)
           collect buf))

;;;###autoload
(defun +buffer-buried-list (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-remove-if #'get-buffer-window (or buffer-list (+buffer-list))))

;;;###autoload
(defun +buffer-matching-list (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (+buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun +buffer-set-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq +buffer-real-p flag)))

;;;###autoload
(defun +buffer-kill-with-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun +buffer-kill-matching (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that
match the regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (+buffer-matching-list pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))

;;
;; Hooks

;;;###autoload
(defun +buffer|mark-as-real ()
  "Hook function that marks the current buffer as real."
  (+buffer-set-real (current-buffer) t))

;;
;; Advice

;;;###autoload
(defun +buffer*switch-to-fallback-maybe (orig-fn)
  "Advice for `kill-this-buffer'. If in a dedicated window,
delete it. If there are no real buffers left OR if all remaining
buffers are visible in other windows, switch to
`+buffer-fallback'. Otherwise, delegate to original
`kill-this-buffer'."
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window))
          ((eq buf (+buffer-fallback))
           (message "Can't kill the fallback buffer."))
          ((+buffer-real-p buf)
           (if (and buffer-file-name
                    (buffer-modified-p buf)
                    (not (y-or-n-p
                          (format "Buffer %s is modified; kill anyway?" buf))))
               (message "Aborted")
             (set-buffer-modified-p nil)
             (when (or ;; if there aren't more real buffers than visible buffers,
                    ;; then there are no real, non-visible buffers left.
                    (not (cl-set-difference (+buffer-real-list)
                                            (+buffer-visible-list)))
                    ;; if we end up back where we start (or previous-buffer
                    ;; returns nil), we have nowhere left to go
                    (memq (previous-buffer) (list buf 'nil)))
               (switch-to-buffer (+buffer-fallback)))
             (kill-buffer buf)))
          ((funcall orig-fn)))))

;;
;; Interactive commands

;;;###autoload
(defun +buffer/kill-this-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing
this buffer have switched to a real buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (let ((windows (get-buffer-window-list buffer nil t)))
    (when (and (buffer-modified-p buffer) dont-save)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)))
    (kill-buffer buffer)
    (cl-loop for win in windows
             if (+buffer-real-p (window-buffer win))
             do (with-selected-window win (previous-buffer)))))

;;;###autoload
(defun +buffer/kill-all (&optional project-p)
  "Kill all buffers and closes their windows.

If PROJECT-P (universal argument), don't close windows and only
kill buffers that belong to the current project."
  (interactive "P")
  (unless project-p
    (delete-other-windows))
  (switch-to-buffer (+buffer-fallback))
  (+buffer/cleanup-session (if project-p (+project-buffer-list))))

;;;###autoload
(defun +buffer/kill-other (&optional project-p)
  "Kill all other buffers (besides the current one).

If PROJECT-P (universal argument), kill only buffers that belong
to the current project."
  (interactive "P")
  (let ((buffers (if project-p (+project-buffer-list) (+buffer-list)))
        (current-buffer (current-buffer)))
    (dolist (buf buffers)
      (unless (eq buf current-buffer)
        (+buffer-kill-with-windows buf)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" (length buffers)))))

;;;###autoload
(defun +buffer/kill-matching (pattern &optional project-p)
  "Kill buffers that match PATTERN in BUFFER-LIST.

If PROJECT-P (universal argument), only kill matching buffers in
the current project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         current-prefix-arg))
  (let* ((buffers (if project-p (+project-buffer-list) (+buffer-list)))
         (n (+buffer-kill-matching pattern buffers)))
    (when (called-interactively-p 'interactive)
      (message "Killed %s buffers" n))))

;;;###autoload
(defun +buffer/cleanup-session (&optional buffer-list)
  "Clean up buried buries and orphaned processes in the current
workspace. If ALL-P (universal argument), clean them up
globally."
  (interactive)
  (let ((buffers (+buffer-buried-list buffer-list))
        (n 0))
    (dolist (buf buffers)
      (unless (buffer-modified-p buf)
        (kill-buffer buf)
        (cl-incf n)))
    (setq n (+ n (+buffer/cleanup-processes)))
    (dolist (hook +buffer-cleanup-session-hook)
      (let ((m (funcall hook)))
        (when (integerp m)
          (setq n (+ n m)))))
    (message "Cleaned up %s buffers" n)
    n))

;;;###autoload
(defun +buffer/cleanup-processes ()
  "Kill all processes that have no visible associated buffers.
Return number of processes killed."
  (interactive)
  (let ((n 0))
    (dolist (p (process-list))
      (let ((process-buffer (process-buffer p)))
        (when (and (process-live-p p)
                   (not (string= (process-name p) "server"))
                   (or (not process-buffer)
                       (and (bufferp process-buffer)
                            (not (buffer-live-p process-buffer)))))
          (delete-process p)
          (cl-incf n))))
    n))

;;;###autoload
(defun +buffer/yank-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))
