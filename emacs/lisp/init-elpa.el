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

(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" path-packages-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(autoload 'elpaca--queue "elpaca")      ; needed because of byte-compilation of this file
(elpaca `(,@elpaca-order))

(when elpa-bootstrap-p
  (elpaca-generate-autoloads "init" (expand-file-name "lisp/" path-emacs-dir)))

(when elpa-bootstrap-p
  (defun elpa--log-wrapper (fn &rest args)
    "Add messages buffer logging to `elpaca--log'.

FN is called with ARGS."
    (let ((e (nth 0 args))
          (txt (nth 1 args)))
      (message "[%s] %s" (elpaca<-id e) txt))
    (apply fn args))
  (advice-add 'elpaca--log :around #'elpa--log-wrapper))


;; Install `use-package' support

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(setq-default use-package-enable-imenu-support t)

(elpaca-wait)


;; critical packages

(use-package s)
(use-package dash)
(use-package emacsql)
(use-package emacsql-sqlite-builtin)

(elpaca-wait)


;; 'common' packages

(use-package async
  :defer t)

(use-package ts
  :defer t)

(use-package request
  :defer t
  :init
  (setq-default
   request-storage-directory (expand-file-name "request" path-cache-dir)))

(use-package request-deferred
  :defer t)

(use-package esup
  :defer t
  :init
  ;; https://github.com/progfolio/elpaca/issues/23
  (setq esup-depth 0))



(provide 'init-elpa)
;;; init-elpa.el ends here
