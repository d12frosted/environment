;;; init-package.el --- package system -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Oct 2019
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

(require 'init-path)

(defvar +benchmark-enable nil
  "Benchmark loading when non-nil.")

;; Setup `straight' package manager.
(setq-default straight-repository-branch "develop"
              straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" +path-packages-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (eval-and-compile
    (setq straight-base-dir +path-packages-dir)
    (setq straight-use-package-by-default t
          straight-repository-branch "develop"))
  (load bootstrap-file nil 'nomessage))

(require 'straight)
(straight-use-package 'use-package)
(straight-use-package 'el-patch)

(defun +package-install ()
  "Install missing packages."
  (interactive)
  (straight-check-all))

(defun +package-upgrade ()
  "Upgrade installed packages."
  (interactive)
  (+package-fetch-all)
  (straight-merge-all)
  (delete-file (concat +path-packages-dir "straight/build-cache.el"))
  (delete-directory (concat +path-packages-dir "straight/build") 'recursive)
  (+package-install))

(defun +package-fetch-all (&optional from-upstream predicate)
  "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched."
  (interactive "P")
  (straight--map-existing-repos-interactively
   (lambda (package)
     (+package-fetch-package package from-upstream 3))
   predicate))

(defun +package-fetch-package (package &optional from-upstream n)
  "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

N is for retries."
  (condition-case err
      (straight-fetch-package package from-upstream)
    (error
     (if (> n 0)
         (+package-fetch-package package from-upstream (- n 1))
       (signal (car err) (cdr err))))))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))
(require 'use-package)
(require 'el-patch)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; User heavily used packages
(use-package s)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Initialization benchmark
(when +benchmark-enable
  (use-package benchmark-init
    :defines swiper-font-lock-exclude
    :commands (benchmark-init/activate)
    :hook (after-init . benchmark-init/deactivate)
    :init (benchmark-init/activate)
    :config
    (with-eval-after-load 'swiper
      (add-to-list 'swiper-font-lock-exclude 'benchmark-init/tree-mode))))

(provide 'init-package)
;;; init-package.el ends here
