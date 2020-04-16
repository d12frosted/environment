;;; +org-notes.el --- note taking helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 03 Apr 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defvar +org-notes-directory nil)

(defun +org-notes-list ()
  "Open a list of notes."
  (interactive)
  (call-interactively #'deft)
  (call-interactively #'deft-refresh))

(defun +org-notes-today ()
  "Open today's note."
  (interactive)
  (call-interactively #'org-roam-today))

(defun +org-notes-yesterday ()
  "Open yesterday's note."
  (interactive)
  (call-interactively #'org-roam-yesterday))

(defun +org-notes-tomorrow ()
  "Open tomorrow's note."
  (interactive)
  (call-interactively #'org-roam-tomorrow))

(defun +org-notes-find ()
  "Find a note."
  (interactive)
  (call-interactively #'org-roam-find-file))

(defun +org-notes-insert ()
  "Insert a link to the note."
  (interactive)
  (call-interactively #'org-roam-insert))

(defun +org-notes-setup-buffer (&optional _)
  "Setup current buffer for notes viewing and editing.

If the current buffer is not a note, does nothing."
  (unless (active-minibuffer-window)
    (if (and buffer-file-name
             (string-equal +org-notes-directory
                           (file-name-directory buffer-file-name)))
        (progn
          (unless (bound-and-true-p org-roam-mode)
            (org-roam-mode 1))
          (setq-local time-stamp-start "#\\+TIME-STAMP:[ 	]+\\\\?[\"<]+")
          (setq org-roam-last-window (get-buffer-window))
          (unless (eq 'visible (org-roam--current-visibility))
            (delete-other-windows)
            (call-interactively #'org-roam)))
      (when (and (fboundp #'org-roam--current-visibility)
                 (eq 'visible (org-roam--current-visibility)))
        (delete-window (get-buffer-window org-roam-buffer))))))

(defun +org-notes-rebuild ()
  "Rebuild notes database."
  (interactive)
  (org-roam-db--clear)
  (org-roam-db-build-cache))

(provide '+org-notes)
;;; +org-notes.el ends here
