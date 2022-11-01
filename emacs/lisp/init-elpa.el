;;; init-elpa.el --- Initialize ELPA -*- lexical-binding: t; -*-
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
;; Setup Emacs for installing packages from MELPA and Git
;; repositories. Enable configuration via `use-package'.
;;
;;; Code:

(require 'config-path)

(defvar elpa-bootstrap-p nil)



(setq package-user-dir
      (expand-file-name
       "elpa/"
       path-packages-dir))


;; bootstrap `elpaca'

(declare-function elpaca-generate-autoloads "elpaca")
(defvar elpaca-directory (expand-file-name "elpaca/" path-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(when-let ((elpaca-repo (expand-file-name "repos/elpaca/" elpaca-directory))
           (elpaca-build (expand-file-name "elpaca/" elpaca-builds-directory))
           (elpaca-target (if (file-exists-p elpaca-build) elpaca-build elpaca-repo))
           (elpaca-url  "https://www.github.com/progfolio/elpaca.git")
           ((add-to-list 'load-path elpaca-target))
           ((not (file-exists-p elpaca-repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (progn
        (unless (zerop (call-process "git" nil buffer t "clone" elpaca-url elpaca-repo))
          (error "%s" (list (with-current-buffer buffer (buffer-string)))))
        (byte-recompile-directory elpaca-repo 0 'force)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" elpaca-repo)
        (kill-buffer buffer))
    ((error)
     (delete-directory elpaca-directory 'recursive)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (format "\n%S" err))
       (display-buffer buffer)))))
(require 'elpaca-autoloads)
(autoload 'elpaca--queue "elpaca")      ; needed because of byte-compilation of this file
(elpaca (elpaca :host github :repo "progfolio/elpaca"))

(when elpa-bootstrap-p
  (elpaca-generate-autoloads "init" (expand-file-name "lisp/" path-emacs-dir)))



(defun elpa-block-until-ready ()
  "Block Emacs until all packages are installed.

Unfortunately, `elpaca' is asynchronous-only, but there are
flows (like scripts using `init'), where you need to do perform
some actions *when* environment is ready."
  (if env-graphic-p
      (add-hook 'after-init-hook #'elpaca-process-queues)
    (elpaca-process-queues))
  (unless env-graphic-p
    (while (cl-find 'incomplete (reverse elpaca--queues) :key #'elpaca-q<-status)
      (message "waiting for installation to complete...")
      (sit-for 0.2))))

(defmacro elpa-require (pkg)
  "Bootstrap PKG and require it."
  `(elpaca ,pkg (require ',(elpaca--first pkg))))

(defalias #'elpa-use-package #'elpaca-use-package)


;; critical packages

(setq-default use-package-enable-imenu-support t)

(elpa-require use-package)
(elpa-require s)
(elpa-require dash)

;; (message "%s" (string-join load-path "\n"))


;; popular packages

(elpa-use-package async
  :defer t)

(elpa-use-package ts
  :defer t)

(elpa-use-package request
  :defer t
  :init
  (setq-default
   request-storage-directory (expand-file-name "request" path-cache-dir)))

(elpa-use-package request-deferred
  :defer t)



;; profiler
(elpa-use-package esup
  :defer t
  :init
  ;; https://github.com/progfolio/elpaca/issues/23
  (setq esup-depth 0))



(provide 'init-elpa)
;;; init-elpa.el ends here
