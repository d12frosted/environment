;;; scratch.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;; Most of the code was borrowed from hlissner/doom-emacs.
;;
;;; Commentary:
;;
;;; Code:

;; TODO: move to the module

(defvar nucleus-scratch-files-dir (concat nucleus-etc-dir "scratch/")
  "Where to store project scratch files, created by
`nucleus/open-project-scratch-buffer'.")

(defvar nucleus-scratch-buffer-display-fn #'display-buffer
  "The function to use to display the scratch buffer. Must accept
one argument: the buffer to display.")

(defvar nucleus-scratch-buffer-major-mode nil
  "What major mode to use in scratch buffers. This can be one of the
following:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar nucleus-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is made.")

;;
;; Library

;;;###autoload
(defun nucleus-scratch-buffer (&optional file mode text)
  "Return a scratchpad buffer in major MODE with TEXT in it.

If FILE is a valid path, open it as if it were a persistent
scratchpad."
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
      (run-hooks 'nucleus-scratch-buffer-hook)
      (current-buffer))))

;;;###autoload
(defun nucleus/open-scratch-buffer (&optional arg)
  "Opens a scratch pad window in the same major-mode.

If ARG (universal argument), then open a persistent scratch pad
buffer. You'll be prompted for its name, or to open a previously
created. These are stored in `nucleus-scratch-files-dir'.

If a region is active, copy its contents to the scratch pad."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     nucleus-scratch-buffer-display-fn
     (nucleus-scratch-buffer
      (when arg
        (if-let* ((file (read-file-name "Open scratch file > " nucleus-scratch-files-dir "scratch")))
            file
          (user-error "Aborting")))
      (cond ((eq nucleus-scratch-buffer-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null nucleus-scratch-buffer-major-mode) nil)
            ((symbolp nucleus-scratch-buffer-major-mode)
             nucleus-scratch-buffer-major-mode))
      (and (region-active-p)
           (buffer-substring-no-properties
            (region-beginning) (region-end)))))))

;;;###autoload
(defun nucleus/switch-to-scratch-buffer (&optional arg)
  "Switches to a scratch pad buffer in the current window.

Otherwise, does exactly what `nucleus/open-scratch-buffer' does."
  (interactive "P")
  (let ((nucleus-scratch-buffer-display-fn #'switch-to-buffer))
    (nucleus/open-scratch-buffer arg)))

;;;###autoload
(defun nucleus/delete-scratch-files ()
  "Deletes all scratch buffers in `nucleus-scratch-files-dir'."
  (interactive)
  (dolist (file (directory-files nucleus-scratch-files-dir t "^[^.]" t))
    (delete-file file)
    (message "Deleted '%s'" (file-name-nondirectory file))))
