;;; init-buffer.el --- Buffer configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Buffer configurations and utilities.
;;
;;; Code:

(require 'config-path)
(require 'init-kbd)



(defvar buffer-fallback-name "*scratch*"
  "The name of the buffer to fall back to.

Used when no other buffers exist.")

(defun buffer-fallback ()
  "Return the fallback buffer, creating it if necessary.

By default this is the scratch buffer. See
`buffer-fallback-name' to change this."
  (get-buffer-create buffer-fallback-name))



(defun buffer-display-and-switch (buffer-or-name)
  "Display BUFFER-OR-NAME in some window and switch to it."
  (select-window (display-buffer buffer-or-name)))



(defvar buffer-messages-display-fn #'buffer-display-and-switch
  "The function to use to display messages buffer.

Must accept one argument: the buffer to display.")

(defvar buffer-messages-name "*Messages*"
  "The name of the messages buffer.")

(defun buffer-pop-messages ()
  "Open `buffer-messages-name' buffer."
  (interactive)
  (funcall buffer-messages-display-fn
           (get-buffer-create buffer-messages-name)))

(defun buffer-switch-to-messages ()
  "Open `buffer-messages-name' buffer."
  (interactive)
  (let ((buffer-messages-display-fn #'switch-to-buffer))
    (buffer-pop-messages)))



(defvar buffer-scratch-files-dir (concat path-etc-dir "scratch/")
  "Where to store project scratch files.

Files are created by `buffer-open-project-scratch'.")

(defvar buffer-scratch-display-fn #'buffer-display-and-switch
  "The function to use to display the scratch buffer.

Must accept one argument: the buffer to display.")

(defvar buffer-scratch-major-mode nil
  "What major mode to use in scratch buffers.

This can be one of the following:

  t          Inherits the major mode of the last buffer you had
             selected.
  nil        Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar buffer-scratch-hook ()
  "The hooks to run after a scratch buffer is made.")

(defun buffer-scratch (&optional file mode text)
  "Return a scratch buffer in major MODE with TEXT in it.

If FILE is a valid path, open it as if it were a persistent
scratch."
  (if file (setq file (file-truename file)))
  (let ((buffer
         (if file
             (with-current-buffer (find-file-noselect file)
               (rename-buffer (format "*scratch (%s)*"
                                      (file-name-nondirectory file)))
               (current-buffer))
           (get-buffer-create "*scratch*"))))
    (with-current-buffer buffer
      (when (and (functionp mode)
                 (not (eq major-mode mode)))
        (funcall mode))
      (when text
        (insert text))
      (run-hooks 'buffer-scratch-hook)
      (current-buffer))))

(defun buffer-pop-scratch (&optional arg)
  "Opens a scratch pad window in the same `major-mode'.

If ARG (universal argument), then open a persistent scratch
buffer. You'll be prompted for its name, or to open a previously
created. These are stored in `buffer-scratch-files-dir'.

If a region is active, copy its contents to the scratch."
  (interactive "P")
  (funcall
   buffer-scratch-display-fn
   (buffer-scratch
    (when arg
      (if-let* ((file (read-file-name
                       "Open scratch file > "
                       buffer-scratch-files-dir
                       "scratch")))
          file
        (user-error "Aborting")))
    (cond ((eq buffer-scratch-major-mode t)
           (unless (or buffer-read-only
                       (derived-mode-p 'special-mode)
                       (string-match-p "^ ?\\*" (buffer-name)))
             major-mode))
          ((null buffer-scratch-major-mode) nil)
          ((symbolp buffer-scratch-major-mode)
           buffer-scratch-major-mode))
    (and (region-active-p)
         (buffer-substring-no-properties
          (region-beginning) (region-end))))))

(defun buffer-switch-to-scratch (&optional arg)
  "Switch to a scratch buffer in the current window.

Otherwise, does exactly what `buffer-pop-scratch' does.

ARG is passed to `buffer-pop-scratch'."
  (interactive "P")
  (let ((buffer-scratch-display-fn #'switch-to-buffer))
    (buffer-pop-scratch arg)))

(defun buffer-delete-scratch-files ()
  "Deletes all scratch buffers in `buffer-scratch-files-dir'."
  (interactive)
  (dolist (file (directory-files buffer-scratch-files-dir
                                 t "[^.]" t))
    (delete-file file)
    (message "Deleted '%s'" (file-name-nondirectory file))))



(defun buffer-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit. Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))



(leader-def
  "b" '(nil :which-key "buffer...")
  "bM" '(buffer-switch-to-messages
         :which-key "switch to messages buffer")
  "bS" '(buffer-sudo-edit :which-key "sudo edit this file")
  "bX" '(buffer-switch-to-scratch :which-key "pop scratch buffer")
  "bb" '(switch-to-buffer :which-key "switch buffer")
  "bk" '(kill-this-buffer :which-key "kill buffer")
  "bm" '(buffer-pop-messages :which-key "pop messages buffer")
  "bs" '(save-buffer :which-key "save buffer")
  "bx" '(buffer-pop-scratch :which-key "pop scratch buffer"))



(provide 'init-buffer)
;;; init-buffer.el ends here
