;;; init-env.el --- environment configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'use-package)

(defconst +sys-mac-p (eq system-type 'darwin))
(defconst +sys-linux-p (eq system-type 'gnu/linux))
(defconst +sys-graphic-p (display-graphic-p))
(defconst +sys-rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")
(defconst +sys-name (system-name)
  "Name of the system (e.g. hostname).")

(use-package exec-path-from-shell
  :if (and +sys-mac-p +sys-graphic-p)
  :commands (exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-shell-name "/usr/local/bin/fish"
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-debug nil)
  (exec-path-from-shell-initialize))

(provide 'init-env)
;;; init-env.el ends here
