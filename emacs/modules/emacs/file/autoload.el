;;; emacs/file/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
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

;;
;; Public library

;;
;; Helpers

(defun +file--forget (old-path &optional new-path)
  "Ensure `recentf', `projectile' and `save-place' forget OLD-PATH."
  (when (bound-and-true-p recentf-mode)
    (when new-path
      (recentf-add-file new-path))
    (recentf-remove-if-non-kept old-path))
  (when (and (bound-and-true-p projectile-mode)
             (+project-p)
             (projectile-file-cached-p old-path (+project-root)))
    (projectile-purge-file-from-cache old-path))
  (when (bound-and-true-p save-place-mode)
    (save-place-forget-unreadable-files)))

(defun +file--update (path)
  (when (featurep 'vc)
    (vc-file-clearprops path)
    (vc-resynch-buffer path nil t))
  (when (featurep 'magit)
    (magit-refresh)))

(defun +file--copy (old-path new-path &optional force-p)
  (let* ((new-path (expand-file-name new-path))
         (old-path (file-truename old-path))
         (new-path (apply #'expand-file-name
                          (if (or (directory-name-p new-path)
                                  (file-directory-p new-path))
                              (list (file-name-nondirectory old-path) new-path)
                            (list new-path))))
         (new-path-dir (file-name-directory new-path))
         (project-root (+project-root))
         (short-new-name (if (and project-root (file-in-directory-p new-path project-root))
                             (file-relative-name new-path project-root)
                           (abbreviate-file-name new-path))))
    (unless (file-directory-p new-path-dir)
      (make-directory new-path-dir t))
    (when (buffer-modified-p)
      (save-buffer))
    (cond ((file-equal-p old-path new-path)
           (throw 'status 'overwrite-self))
          ((and (file-exists-p new-path)
                (not force-p)
                (not (y-or-n-p (format "File already exists at %s, overwrite?" short-new-name))))
           (throw 'status 'aborted))
          ((file-exists-p old-path)
           (copy-file old-path new-path t)
           short-new-name)
          (short-new-name))))

;;
;; Commands

;;;###autoload
(defun +file/delete-this (&optional path force-p)
  "Delete FILENAME (defaults to the file associated with current buffer) and
kills the buffer. If FORCE-P, force the deletion (don't ask for confirmation)."
  (interactive
   (list (file-truename (buffer-file-name))
         current-prefix-arg))
  (let* ((fbase (file-name-sans-extension (file-name-nondirectory path)))
         (buf (current-buffer)))
    (cond ((not (file-exists-p path))
           (error "File doesn't exist: %s" path))
          ((not (or force-p (y-or-n-p (format "Really delete %s?" fbase))))
           (message "Aborted")
           nil)
          ((unwind-protect
               (progn (delete-file path) t)
             (let ((short-path (file-relative-name path (+project-root))))
               (if (file-exists-p path)
                   (error "Failed to delete %s" short-path)
                 ;; Ensures that windows displaying this buffer will be switched
                 ;; to real buffers (`nucleus-real-buffer-p')
                 (nucleus/kill-this-buffer-in-all-windows buf t)
                 (+file--forget path)
                 (+file--update path)
                 (message "Successfully deleted %s" short-path))))))))

;;;###autoload
(defun +file/copy-this (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH. If FORCE-P, overwrite the destination
file if it exists, without confirmation."
  (interactive "F")
  (pcase (catch 'status
           (when-let* ((dest (+file--copy (buffer-file-name) new-path force-p)))
             (+file--update new-path)
             (message "File successfully copied to %s" dest)))
    (`overwrite-self (error "Cannot overwrite self"))
    (`aborted (message "Aborted"))
    (_ t)))

;;;###autoload
(defun +file/move-this (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH. If FORCE-P, overwrite the destination
file if it exists, without confirmation."
  (interactive "FP")
  (pcase (catch 'status
           (let ((old-path (buffer-file-name))
                 (new-path (expand-file-name new-path)))
             (when-let* ((dest (+file--copy old-path new-path force-p)))
               (when (file-exists-p old-path)
                 (delete-file old-path))
               (kill-this-buffer)
               (find-file new-path)
               (+file--forget old-path new-path)
               (+file--update new-path)
               (message "File successfully moved to %s" dest))))
    (`overwrite-self (error "Cannot overwrite self"))
    (`aborted (message "Aborted"))
    (_ t)))

;;;###autoload
(defun +file/sudo-find (file)
  "Open FILE as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":" (file-remote-p file 'user) "@" (file-remote-p file 'host)  "|sudo:root@" (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;;;###autoload
(defun +file/sudo-this ()
  "Open the current file as root."
  (interactive)
  (+file/sudo-find (file-truename buffer-file-name)))
