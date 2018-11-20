;;; packages.el --- the heart of every cell -*- lexical-binding: t;lexical-binding: t; -*-
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

(dispatcher! (install i) (nucleus--do #'nucleus-packages-install)
  "Installs requested packages that aren't installed.")

(dispatcher! (update u) (nucleus--do #'nucleus-packages-update)
  "Updates packages.")

(dispatcher! (autoremove r) (nucleus--do #'nucleus-packages-autoremove)
  "Removes packages that are no longer needed.")

;;
;; Helpers

(defsubst nucleus--do (fn)
  (nucleus-reload-nucleus-autoloads)
  (when (funcall fn nucleus-auto-accept)
    (nucleus-reload-package-autoloads)))

;;
;; Library

(defun nucleus-packages-install (&optional auto-accept-p)
  "Interactive command for installing missing packages."
  (print! "Looking for packages to install...")
  (let ((packages (nucleus-get-missing-packages)))
    (cond ((not packages)
           (print! (green "No packages to install!"))
           nil)

          ((not (or auto-accept-p
                    (y-or-n-p
                     (format "%s packages will be installed:\n\n%s\n\nProceed?"
                             (length packages)
                             (mapconcat
                              (lambda (pkg)
                                (format "+ %s (%s)"
                                        (car pkg)
                                        (cond ((nucleus-package-different-recipe-p (car pkg))
                                               "new recipe")
                                              ((nucleus-package-different-backend-p (car pkg))
                                               (if (plist-get (cdr pkg) :recipe)
                                                   "ELPA->QUELPA"
                                                 "QUELPA->ELPA"))
                                              ((plist-get (cdr pkg) :recipe)
                                               "QUELPA")
                                              ("ELPA"))))
                              (cl-sort (cl-copy-list packages) #'string-lessp
                                       :key #'car)
                              "\n")))))
           (user-error "Aborted!"))

          ((let (success)
             (nucleus-refresh-packages-maybe nucleus-debug-mode)
             (dolist (pkg packages)
               (print! "Installing %s" (car pkg))
               (nucleus--condition-case!
                (let ((result
                       (or (and (nucleus-package-installed-p (car pkg))
                                (not (nucleus-package-different-backend-p (car pkg)))
                                (not (nucleus-package-different-recipe-p (car pkg)))
                                'already-installed)
                           (and (nucleus-install-package (car pkg) (cdr pkg))
                                (setq success t)
                                'success)
                           'failure))
                      (pin-label
                       (and (plist-member (cdr pkg) :pin)
                            (format " [pinned: %s]" (plist-get (cdr pkg) :pin)))))
                  (print! "%s%s"
                          (pcase result
                            (`already-installed (dark (white "⚠ ALREADY INSTALLED")))
                            (`success (green "✓ DONE"))
                            (`failure (red "✕ FAILED")))
                          (or pin-label "")))))
             (print! (bold (green "Finished!")))
             (when success
               (set-file-times nucleus-packages-dir)
               (nucleus-delete-autoloads-file nucleus-package-autoload-file))
             success)))))

(defun nucleus-packages-update (&optional auto-accept-p)
  "Interactive command for updating packages."
  (print! "Looking for outdated packages...")
  (let ((packages (cl-sort (cl-copy-list (nucleus-get-outdated-packages)) #'string-lessp
                           :key #'car)))
    (cond ((not packages)
           (print! (green "Everything is up-to-date"))
           nil)

          ((not (or auto-accept-p
                    (y-or-n-p
                     (format "%s packages will be updated:\n\n%s\n\nProceed?"
                             (length packages)
                             (let ((max-len
                                    (or (car (sort (mapcar (lambda (it) (length (symbol-name (car it)))) packages)
                                                   #'>))
                                        10)))
                               (mapconcat
                                (lambda (pkg)
                                  (format (format "+ %%-%ds %%-%ds -> %%s" (+ max-len 2) 14)
                                          (symbol-name (car pkg))
                                          (package-version-join (cadr pkg))
                                          (package-version-join (cl-caddr pkg))))
                                packages
                                "\n"))))))
           (user-error "Aborted!"))

          ((let (success)
             (dolist (pkg packages)
               (print! "Updating %s" (car pkg))
               (nucleus--condition-case!
                (print!
                 (let ((result (nucleus-update-package (car pkg) t)))
                   (when result (setq success t))
                   (color (if result 'green 'red)
                          (if result "✓ DONE" "✕ FAILED"))))))
             (print! (bold (green "Finished!")))
             (when success
               (set-file-times nucleus-packages-dir)
               (nucleus-delete-autoloads-file nucleus-package-autoload-file))
             success)))))

(defun nucleus-packages-autoremove (&optional auto-accept-p)
  "Interactive command for auto-removing orphaned packages."
  (print! "Looking for orphaned packages...")
  (let ((packages (nucleus-get-orphaned-packages)))
    (cond ((not packages)
           (print! (green "No unused packages to remove"))
           nil)

          ((not
            (or auto-accept-p
                (y-or-n-p
                 (format "%s packages will be deleted:\n\n%s\n\nProceed?"
                         (length packages)
                         (mapconcat
                          (lambda (sym)
                            (let ((backend (nucleus-package-backend sym)))
                              (format "+ %s (%s)" sym
                                      (if (nucleus-package-different-backend-p sym)
                                          (pcase backend
                                            (`quelpa "QUELPA->ELPA")
                                            (`elpa "ELPA->QUELPA")
                                            (_ "removed"))
                                        (upcase (symbol-name backend))))))
                          (sort (cl-copy-list packages) #'string-lessp)
                          "\n")))))
           (user-error "Aborted!"))

          ((let (success)
             (dolist (pkg packages)
               (nucleus--condition-case!
                (let ((result (nucleus-delete-package pkg t)))
                  (if result (setq success t))
                  (print! (color (if result 'green 'red) "%s %s")
                          (if result "✓ Removed" "✕ Failed to remove")
                          pkg))))
             (print! (bold (green "Finished!")))
             (when success
               (set-file-times nucleus-packages-dir)
               (nucleus-delete-autoloads-file nucleus-package-autoload-file))
             success)))))
