;;; init-buffer.el --- buffer -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
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

(require 'init-path)
(require 'init-keybindings)
(require 'subr-x)

(+leader-def
  "b" '(nil :which-key "buffer...")
  "bb" '(switch-to-buffer :which-key "Switch buffer")
  "bk" '(kill-this-buffer :which-key "Kill buffer")
  "bs" '(save-buffer :which-key "Save buffer")
  "bx" '(+buffer/pop-scratch :which-key "Pop scratch buffer")
  "bX" '(+buffer/switch-to-scratch :which-key "Pop scratch buffer")
  "bm" '(+buffer/pop-messages :which-key "Pop messages buffer")
  "bM" '(+buffer/switch-to-messages :which-key "Switch to messages buffer")
  "bS" '(+file/sudo-this :which-key "Sudo edit this file"))

;;;###autoload
(defvar +buffer-fallback-name "*scratch*"
  "The name of the buffer to fall back to.

Used when no other buffers exist.")

;;;###autoload
(defun +buffer-fallback ()
  "Return the fallback buffer, creating it if necessary.

By default this is the scratch buffer. See
`+buffer-fallback-name' to change this."
  (get-buffer-create +buffer-fallback-name))

;;;###autoload
(defun +buffer-display-and-switch (buffer-or-name)
  "Display BUFFER-OR-NAME in some window and switch to it."
  (select-window (display-buffer buffer-or-name)))

(defvar +buffer-messages-display-fn #'+buffer-display-and-switch
  "The function to use to display messages buffer.

Must accept one argument: the buffer to display.")

;;;###autoload
(defvar +buffer-messages-name "*Messages*"
  "The name of the messages buffer.")

;;;###autoload
(defun +buffer/pop-messages ()
  "Open `+buffer-messages-name' buffer."
  (interactive)
  (funcall +buffer-messages-display-fn
           (get-buffer-create +buffer-messages-name)))

;;;###autoload
(defun +buffer/switch-to-messages ()
  "Open `+buffer-messages-name' buffer."
  (interactive)
  (let ((+buffer-messages-display-fn #'switch-to-buffer))
    (+buffer/pop-messages)))

(defvar +buffer-scratch-files-dir (concat +path-etc-dir "scratch/")
  "Where to store project scratch files.

Files are created by `+buffer/open-project-scratch'.")

(defvar +buffer-scratch-display-fn #'+buffer-display-and-switch
  "The function to use to display the scratch buffer.

Must accept one argument: the buffer to display.")

(defvar +buffer-scratch-major-mode nil
  "What major mode to use in scratch buffers.

This can be one of the following:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar +buffer-scratch-hook ()
  "The hooks to run after a scratch buffer is made.")

;;;###autoload
(defun +buffer-scratch (&optional file mode text)
  "Return a scratch buffer in major MODE with TEXT in it.

If FILE is a valid path, open it as if it were a persistent
scratch."
  (if file (setq file (file-truename file)))
  (let ((buffer
         (if file
             (with-current-buffer (find-file-noselect file)
               (rename-buffer (format "*scratch (%s)*" (file-name-nondirectory file)))
               (current-buffer))
           (get-buffer-create "*scratch*"))))
    (with-current-buffer buffer
      (when (and (functionp mode)
                 (not (eq major-mode mode)))
        (funcall mode))
      (when text
        (insert text))
      (run-hooks '+buffer-scratch-hook)
      (current-buffer))))

;;;###autoload
(defun +buffer/pop-scratch (&optional arg)
  "Opens a scratch pad window in the same `major-mode'.

If ARG (universal argument), then open a persistent scratch
buffer. You'll be prompted for its name, or to open a previously
created. These are stored in `+buffer-scratch-files-dir'.

If a region is active, copy its contents to the scratch."
  (interactive "P")
  (funcall
   +buffer-scratch-display-fn
   (+buffer-scratch
    (when arg
      (if-let* ((file (read-file-name
                       "Open scratch file > "
                       +buffer-scratch-files-dir
                       "scratch")))
          file
        (user-error "Aborting")))
    (cond ((eq +buffer-scratch-major-mode t)
           (unless (or buffer-read-only
                       (derived-mode-p 'special-mode)
                       (string-match-p "^ ?\\*" (buffer-name)))
             major-mode))
          ((null +buffer-scratch-major-mode) nil)
          ((symbolp +buffer-scratch-major-mode)
           +buffer-scratch-major-mode))
    (and (region-active-p)
         (buffer-substring-no-properties
          (region-beginning) (region-end))))))

;;;###autoload
(defun +buffer/switch-to-scratch (&optional arg)
  "Switch to a scratch buffer in the current window.

Otherwise, does exactly what `+buffer/pop-scratch' does.

ARG is passed to `+buffer/pop-scratch'."
  (interactive "P")
  (let ((+buffer-scratch-display-fn #'switch-to-buffer))
    (+buffer/pop-scratch arg)))

;;;###autoload
(defun +buffer/delete-scratch-files ()
  "Deletes all scratch buffers in `+buffer-scratch-files-dir'."
  (interactive)
  (dolist (file (directory-files +buffer-scratch-files-dir t "^[^.]" t))
    (delete-file file)
    (message "Deleted '%s'" (file-name-nondirectory file))))

(provide 'init-buffer)
;;; init-buffer.el ends here
