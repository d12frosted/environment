;;; d12-macos-reveal.el --- d12-macos-reveal file for personal configurations -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; Package-Version: 0.0.1
;; Package-Requires: ((reveal-in-osx-finder "0.3.3"))
;;
;; This file is not part of GNU Emacs.
;;; License: GPLv3
;;
;;; Commentary:
;;
;; Functions to reveal file or project's root in Finder.
;;
;;; Code:

(require 'reveal-in-osx-finder)

(defun d12-macos-reveal/file ()
  (interactive)
  (reveal-in-osx-finder))

(defun d12-macos-reveal/project-root ()
  (interactive)
  (shell-command (format "open '%s'" (projectile-project-root))))

(provide 'd12-macos-reveal)
;;; d12-macos-reveal.el ends here
