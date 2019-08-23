;;; feature/projects/autoload.el -*- lexical-binding: t; -*-
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
;; Macros

;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. 

This is necessary if you want to interactive with a project other
than the one you're in."
  `(let ((projectile-project-root-cache (make-hash-table :test 'equal))
         projectile-project-name
         projectile-require-project-root)
     ,@body))

;;;###autoload
(defmacro project-file-exists-p! (files)
  "Checks if the project has the specified FILES.

Paths are relative to the project root, unless they start with ./
or ../ (in which case they're relative to
`default-directory'). If they start with a slash, they are
absolute."
  `(file-exists-p! ,files (+project-root)))

;;
;; Commands

;;;###autoload
(defun +project/reload ()
  "Reload the project root cache."
  (interactive)
  (projectile-invalidate-cache nil)
  (setq-default projectile-project-root nil)
  (dolist (fn projectile-project-root-files-functions)
    (remhash (format "%s-%s" fn default-directory)
	           projectile-project-root-cache)))

;;;###autoload
(defun +project/browse-this ()
  "Traverse a file structure starting linearly from
`+project-root'."
  (interactive)
  (+project-browse (+project-root)))

;;
;; Library

;;;###autoload
(defalias '+project-p #'projectile-project-p)

;;;###autoload
(defalias '+project-root #'projectile-project-root)

;;;###autoload
(defun +project-globally-ignore-dirs (&rest args)
  "Add ARGS to `projectile-globally-ignored-directories'."
  (setq projectile-globally-ignored-directories
  	(append projectile-globally-ignored-directories args)))

;;;###autoload
(defun +project-name (&optional dir)
  "Return the name of the current project."
  (let ((project-root (or (projectile-project-root dir)
                          (if dir (expand-file-name dir)))))
    (if project-root
        (funcall projectile-project-name-function project-root)
      "-")))

;;;###autoload
(defun +project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (projectile-project-root dir)))

;;;###autoload
(defun +project-find-file (dir)
  "Fuzzy-find a file under DIR."
  (without-project-cache!
   (let* ((default-directory (file-truename dir))
          projectile-project-root)
     (call-interactively
      ;; completion modules may remap this command
      (or (command-remapping #'projectile-find-file)
          #'projectile-find-file)))))

;;;###autoload
(defun +project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename dir)))
    (call-interactively
     ;; completion modules may remap this command
     (or (command-remapping #'find-file)
         #'find-file))))

;;;###autoload
(defun +project-buffer-list ()
  "Return a list of buffers belonging to the current project.

If no project is active, return all buffers."
  (let ((buffers (+buffer-list)))
    (if-let* ((project-root (+project-root)))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))
