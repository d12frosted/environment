;;; emacs/buffer/autoload/scratch.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Dec 2018
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

(defvar +buffer-scratch-files-dir (concat nucleus-etc-dir "scratch/")
  "Where to store project scratch files, created by
  `+buffer/open-project-scratch'.")

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
               (rename-buffer (format "*nucleus:scratch (%s)*" (file-name-nondirectory file)))
               (current-buffer))
           (get-buffer-create "*nucleus:scratch*"))))
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
  "Opens a scratch pad window in the same major-mode.

If ARG (universal argument), then open a persistent scratch
buffer. You'll be prompted for its name, or to open a previously
created. These are stored in `+buffer-scratch-files-dir'.

If a region is active, copy its contents to the scratch."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     +buffer-scratch-display-fn
     (+buffer-scratch
      (when arg
        (if-let* ((file (read-file-name "Open scratch file > " +buffer-scratch-files-dir "scratch")))
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
            (region-beginning) (region-end)))))))

;;;###autoload
(defun +buffer/switch-to-scratch (&optional arg)
  "Switches to a scratch buffer in the current window.

Otherwise, does exactly what `+buffer/pop-scratch' does."
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
