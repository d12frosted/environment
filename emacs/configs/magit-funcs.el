;;; magit-funcs.el --- funcs file of magit configurations
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

;;; Code

(defun init-git-gutter ()
  "Common initialization of git-gutter."
  (git-gutter-mode)
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (if (and (not git-gutter-use-fringe)
           global-linum-mode)
      (git-gutter:linum-setup)))

(defun git/load-git-gutter ()
  "Lazy load git gutter and choose between fringe and no fringe."
  (when (display-graphic-p) (require 'git-gutter-fringe))
  (init-git-gutter))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun d12/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (magit-gh-pulls-mode)
  (magit-gh-pulls-reload))

(defun d12/git-link-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link)))

(defun d12/git-link-commit-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link-commit)))
