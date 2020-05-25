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

(defvar +package-archives 'upstream
  "Package archives.

Possible values are: upstream, mirror and local.")

(defvar +benchmark-enable nil
  "Benchmark loading when non-nil.")

(defun +package/set-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list (intern (completing-read
                  "Choose package archives: "
                  '(upstream mirror local)))))

  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('upstream
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))
               ,(cons "org"   (concat proto "://orgmode.org/elpa/"))))
            ('mirror
             `(,(cons "gnu"   (concat proto "://gitlab.com/d12frosted/elpa-mirror/raw/master/gnu/"))
               ,(cons "melpa" (concat proto "://gitlab.com/d12frosted/elpa-mirror/raw/master/melpa/"))
               ,(cons "org"   (concat proto "://gitlab.com/d12frosted/elpa-mirror/raw/master/org/"))))
            ('local
             `(,(cons "gnu"   (concat +path-elpa-mirror-dir "gnu/"))
               ,(cons "melpa" (concat +path-elpa-mirror-dir "melpa/"))
               ,(cons "org"   (concat +path-elpa-mirror-dir "org/"))))
            (archives
             (error "Unknown archives: `%s'" archives))))))

(+package/set-archives +package-archives)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq
   package-enable-at-startup nil        ; To prevent initializing twice
   package-user-dir (expand-file-name "elpa" +path-packages-dir)
   package-gnupghome-dir (expand-file-name "gpg" +path-packages-dir))
  (package-initialize))

;; Setup `straight' package manager.
(setq-default straight-repository-branch "develop")
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

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))
(require 'use-package)
(require 'el-patch)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

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
