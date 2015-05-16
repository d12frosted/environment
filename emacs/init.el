;;; init.el --- init file of d12frosted configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Commentary:

;; Private emacs configurations of Boris Buliga.
;;
;; User key prefixes:
;;
;; - C-c a: applications
;; - C-c b: browse
;; - C-c c: current mode commands
;; - C-c h: helm
;; - C-c m: mode manager
;; - C-c o: org mode
;; - C-c p: projectile
;; - C-c t: toggle things and skeletons
;; - C-c u: miscellaneous utilities
;; - C-c v: version control

;;; Code

(require 'cl)

;;; Configurations
;; ================

(defconst d12/configs-directory
  (expand-file-name (concat user-emacs-directory "configs/"))
  "Configs directory.")

(defun d12/load-config (name)
  "Load configuration with NAME.

  Configuration is a set of two files:
  - NAME-funcs.el
  - NAME-configs.el

  that are located in 'd12/configs-directory.

  NAME-funcs.el file is loaded before NAME-configs.el, but
  only if exists."
  (load (concat d12/configs-directory name "-funcs.el") t)
  (load (concat d12/configs-directory name "-configs.el")))

(d12/load-config "core")

;; start server
(require 'server)
(unless (server-running-p) (server-start))
