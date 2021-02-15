;;; init-elpa.el --- Initialize ELPA -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
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
;; Setup Emacs for installing packages from MELPA and Git
;; repositories. Enable configuration via `use-package'.
;;
;;; Code:

(require 'lib-plist)
(require 'init-path)
(require 'package)
(require 'cl-lib)

(defvar elpa-bootstrap-p nil)



(setq package-user-dir
      (expand-file-name
       "elpa/"
       path-packages-dir))



;;; Standard package repositories
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;;; On-demand installation of packages

(defun elpa-require-package (package
                             &optional
                             min-version
                             no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.

If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when elpa-bootstrap-p
    (message (format "elpa-require-package %s%s"
                     package
                     (if min-version
                         (concat " " min-version)
                       ""))))
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (versions (mapcar #'package-desc-version known)))
        (if (cl-some (lambda (v) (version-list-<= min-version v))
                     versions)
            (if min-version
                (package-install-from-archive
                 (cadr (assoc package package-archive-contents)))
              (package-install package))
          (if no-refresh
              (error "No version of %s >= %S is available"
                     package
                     min-version)
            (package-refresh-contents)
            (elpa-require-package package min-version t))))))


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)


;; package.el updates the saved version of package-selected-packages
;; correctly only after custom-file has been loaded, which is a bug.
;; We work around this by adding the required packages to
;; package-selected-packages after startup is complete.

(defvar elpa-required-packages nil)

(defun elpa-note-selected (oldfun package &rest args)
  "Note if OLDFUN reports PACKAGE was successfully installed.

The package name is noted by adding it to
`elpa-required-packages'. This function is used as an advice for
`elpa-require-package', to which ARGS are passed."
  (let ((available (apply oldfun package args)))
    (prog1
        available
      (when available
        (add-to-list 'elpa-required-packages package)))))

(advice-add 'elpa-require-package :around 'elpa-note-selected)

(when (fboundp 'package--save-selected-packages)
  (elpa-require-package 'seq)
  (add-hook 'after-init-hook
            (lambda ()
              (package--save-selected-packages
               (seq-uniq (append elpa-required-packages
                                 package-selected-packages))))))



(let ((package-check-signature nil))
  (elpa-require-package 'gnu-elpa-keyring-update))



(elpa-require-package 'use-package)

(defun elpa-use-package-install (oldfun package &rest args)
  "Automatically install packages configured via `use-package'.

OLDFUN is called wall PACKAGE and rest of the ARGS."
  (when elpa-bootstrap-p
    (message "using %s package" package))

  ;; install package
  (unless (or (plist-get args :quelpa)
              (plist-get args :built-in)
              (plist-get args :ensure))
    (elpa-require-package package (plist-get args :min-version)))

  ;; cleanup custom properties
  (setq args (plist-delete args :built-in))
  (setq args (plist-delete args :min-version))

  ;; return control to `use-package'
  (when elpa-bootstrap-p
    (message "return control flow to use-package for %s" package))
  (apply oldfun package args))

(advice-add 'use-package :around #'elpa-use-package-install)



(setq-default quelpa-dir
              (expand-file-name
               "quelpa/"
               path-packages-dir))

(use-package quelpa
  :defines (quelpa-dir)
  :init
  (setq-default
   quelpa-checkout-melpa-p elpa-bootstrap-p
   quelpa-update-melpa-p elpa-bootstrap-p
   quelpa-autoremove-p nil)
  (add-to-list 'load-path quelpa-dir))

(use-package quelpa-use-package
  :init
  (setq-default
   quelpa-use-package-inhibit-loading-quelpa t))



(use-package auto-package-update
  :defer t)



(provide 'init-elpa)
;;; init-elpa.el ends here
