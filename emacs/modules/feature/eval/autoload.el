;;; feature/eval/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Dec 2018
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
(defun +eval/buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (cond ((assq major-mode +eval-runners)
         (+eval/region (point-min) (point-max)))
        (t (quickrun))))

;;;###autoload
(defun +eval/region (beg end)
  "Evaluate a region between BEG and END and display the output."
  (interactive "r")
  (let ((load-file-name buffer-file-name))
    (if-let* ((runner (cdr (assq major-mode +eval-runners))))
        (funcall runner beg end)
      (quickrun-region beg end))))

;;;###autoload
(defun +eval/region-and-replace (beg end)
  "Evaluation a region between BEG and END, and replace it with
the result."
  (interactive "r")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (kill-region beg end)
         (condition-case nil
             (prin1 (eval (read (current-kill 0)))
                    (current-buffer))
           (error (message "Invalid expression")
                  (insert (current-kill 0)))))
        (t (quickrun-replace-region beg end))))

;;;###autoload
(defvar +eval-runners nil
  "Alist mapping major modes to interactive runner functions.")

;;;###autodef
(defun set-eval-handler! (mode command)
  "Define a code evaluator for major mode MODE with `quickrun'.

1. If COMMAND is an alist, see `quickrun-add-command':
   (quickrun-add-command MODE COMMAND).

2. If MODE is symbol and COMMAND is a symbol, add it to
   `+eval-runners', which is used by `+eval/region'."
  (cond ((symbolp command)
         (push (cons mode command) +eval-runners))
        ((listp command)
         (after! quickrun
           (quickrun-add-command
             (string-remove-suffix "-mode" (if (symbolp mode)
                                               (symbol-name mode)
                                             mode))
             command)))))

;;
;; REPL

(defvar +eval-repl-buffers (make-hash-table :test 'equal)
  "The buffer of the last open repl.")

(define-minor-mode +eval-repl-mode
  "A minor mode for REPL buffers.")

(defun +eval--ensure-in-repl-buffer (&optional command same-window-p)
  (maphash (lambda (key buffer)
             (unless (buffer-live-p buffer)
               (remhash key +eval-repl-buffers)))
           +eval-repl-buffers)
  (let* ((project-root (+project-root))
         (key (cons major-mode project-root))
         (buffer (gethash key +eval-repl-buffers)))
    (cl-check-type buffer (or buffer null))
    (unless (eq buffer (current-buffer))
      (funcall (if same-window-p #'switch-to-buffer #'pop-to-buffer)
               (if (buffer-live-p buffer)
                   buffer
                 (setq buffer
                       (save-window-excursion
                         (if (commandp command)
                             (call-interactively command)
                           (funcall command))))
                 (unless (bufferp buffer)
                   (error "REPL command didn't return a buffer"))
                 (with-current-buffer buffer (+eval-repl-mode +1))
                 (puthash key buffer +eval-repl-buffers)
                 buffer)))
    (with-current-buffer buffer
      (goto-char (if (and (derived-mode-p 'comint-mode)
                          (cdr comint-last-prompt))
                     (cdr comint-last-prompt)
                   (point-max)))
      buffer)))

;;;###autoload
(defun +eval/open-repl (&optional same-window-p)
  "Opens (or reopens) the REPL associated with the current
major-mode and place the cursor at the prompt.

If SAME-WINDOW-P is non-nil, open REPL in current window."
  (interactive "P")
  (if-let* ((command (cdr (assq major-mode +eval-repls))))
      (when (+eval--ensure-in-repl-buffer command same-window-p)
        t)
    (user-error "No REPL is defined for %s" major-mode)))

;;;###autoload
(defun +eval/send-region-to-repl (beg end &optional auto-execute-p)
  "Sends a selected region to it.

If AUTO-EXECUTE-P, then execute it immediately after.

REPL must be open!"
  (interactive "r")
  (let ((selection (buffer-substring-no-properties beg end)))
    (unless (+eval--ensure-in-repl-buffer)
      (error "No REPL open"))
    (when (bound-and-true-p evil-mode)
      (call-interactively #'evil-append-line))
    (insert (string-trim selection))
    (when auto-execute-p
      ;; I don't use `comint-send-input' because different REPLs may have their
      ;; own. So I just emulate the keypress.
      (execute-kbd-macro (kbd "RET")))))

;;;###autoload
(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs.

Used by `+eval/open-repl' and filled with the
`set-repl-handler!'.")

;;;###autodef
(defun set-repl-handler! (modes command)
  "Defines a REPL for MODES.

MODES is either a single major mode symbol or a list of them.
COMMAND is a function that creates and returns the REPL buffer.

COMMAND can either be a function that takes no arguments, or an
interactive command that will be called interactively."
  (dolist (mode (nucleus-enlist modes))
    (setf (alist-get mode +eval-repls) command)))
