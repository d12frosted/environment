;;; config/os/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

;;
;; Environment modification

(defmacro set-env! (&rest _vars)
  "Inject VARS from your shell environment into Emacs.")

;;
;; Mac related configurations

(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier  'alt
        ;; sane trackpad/mouse scroll settings
        mac-redisplay-dont-reset-vscroll t
        ;; do not use smoth scrolling
        mac-mouse-wheel-smooth-scroll nil
        ;; scroll one line at a time
        mouse-wheel-scroll-amount '(1)
        ;; don't accelerate scrolling
        mouse-wheel-progressive-speed nil
        ;; Do not use native fullscreen mode
        ns-use-native-fullscreen nil
        ;; Don't open files from the workspace in a new frame
        ns-pop-up-frames nil)

  ;; Fix the clipboard in terminal or daemon Emacs (non-GUI)
  (when (or (daemonp) (not (display-graphic-p)))
    (add-hook 'nucleus-post-init-hook #'osx-clipboard-mode))

  (when (or (daemonp) (display-graphic-p))
    ;; Syncs ns frame parameters with theme (and fixes mismatching
    ;; text colr in the frame title)
    (require 'ns-auto-titlebar nil t)

    ;; A known problem with GUI Emacs on MacOS (or daemons started via
    ;; launchctl or brew services): it runs in an isolated
    ;; environment, so envvars will be wrong. That includes the PATH
    ;; Emacs picks up. `exec-path-from-shell' fixes this.
    (when (require 'exec-path-from-shell nil t)
      (defun set-env! (&rest vars)
        "Inject VARS from your shell environment into Emacs."
        (exec-path-from-shell-copy-envs vars))
      (setq exec-path-from-shell-shell-name "/usr/local/bin/fish"
	    exec-path-from-shell-check-startup-files nil
            exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments)
            exec-path-from-shell-debug nucleus-debug-mode
            exec-path-from-shell-variables
            (nconc exec-path-from-shell-variables '("LC_CTYPE" "LC_ALL" "LANG")))
      (exec-path-from-shell-initialize))))
