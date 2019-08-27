;;; feature/projects/config.el -*- lexical-binding: t; -*-
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

(def-package! projectile
  :commands (projectile-project-root
	           projectile-project-name
	           projectile-project-p)
  :init
  (setq projectile-cache-file (concat nucleus-cache-dir "projectile.cache")
	      projectile-enable-caching nil ;; (not noninteractive)
	      projectile-known-projects-file (concat nucleus-cache-dir "projectile.projects")
	      projectile-require-project-root t
        projectile-indexing-method 'alien
	      projectile-ignored-projects '("~/" "/tmp"))
  :config
  (when (executable-find "fd")
    (setq projectile-git-command "fd . -t f -0"
          projectile-generic-command projectile-git-command))

  (projectile-mode +1)

  ;; a more generic project root file
  (push ".project" projectile-project-root-files-bottom-up)

  ;; ignore `nucleus-local-dir'
  (+project-globally-ignore-dirs (abbreviate-file-name nucleus-local-dir)))
