;;; lisp/init-env.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
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

(defconst +sys-mac-p (eq system-type 'darwin))
(defconst +sys-linux-p (eq system-type 'gnu/linux))
(defconst +sys-graphic-p (display-graphic-p))
(defconst +sys-rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(use-package exec-path-from-shell
  :if (and +sys-mac-p +sys-graphic-p)
  :init
  (setq exec-path-from-shell-shell-name "/usr/local/bin/fish"
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments)
        exec-path-from-shell-debug nil
        exec-path-from-shell-variables
        (nconc exec-path-from-shell-variables '("LC_CTYPE" "LC_ALL" "LANG")))
  (exec-path-from-shell-initialize))

(provide 'init-env)
;;; init-env.el ends here
