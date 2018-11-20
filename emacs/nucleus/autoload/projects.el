;;; projects.el --- the heart of every cell -*- lexical-binding: t; -*-
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

;; TODO: move me to the modules

;;
;; Macros

;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. This
is necessary if you want to interactive with a project other than
the one you're in."
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
  `(file-exists-p! ,files (nucleus-project-root)))

;;
;; Commands

;;;###autoload
(defun nucleus/reload-project ()
  "Reload the project root cache."
  (interactive)
  (projectile-invalidate-cache nil)
  (setq-default projectile-project-root nil)
  (dolist (fn projectile-project-root-files-functions)
    (remhash (format "%s-%s" fn default-directory) projectile-project-root-cache)))

;;
;; Library

;;;###autoload
(defalias 'nucleus-project-p #'projectile-project-p)

;;;###autoload
(defalias 'nucleus-project-root #'projectile-project-root)

;;;###autoload
(defun nucleus-project-name (&optional dir)
  "Return the name of the current project."
  (let ((project-root (or (projectile-project-root dir)
                          (if dir (expand-file-name dir)))))
    (if project-root
        (funcall projectile-project-name-function project-root)
      "-")))

;;;###autoload
(defun nucleus-project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (projectile-project-root dir)))

;;;###autoload
(defun nucleus-project-find-file (dir)
  "Fuzzy-find a file under DIR."
  (without-project-cache!
   (let* ((default-directory (file-truename dir))
          projectile-project-root)
     (call-interactively
      ;; completion modules may remap this command
      (or (command-remapping #'projectile-find-file)
          #'projectile-find-file)))))

;;;###autoload
(defun nucleus-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename dir)))
    (call-interactively
     ;; completion modules may remap this command
     (or (command-remapping #'find-file)
         #'find-file))))
