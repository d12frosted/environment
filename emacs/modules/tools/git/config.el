;;; tools/git/config.el -*- lexical-binding: t; -*-
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

(def-package! magit
  :init
  ;; we already use `global-auto-revert-mode'
  (setq magit-auto-revert-mode nil)

  ;; full-screen is my friend
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :config
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)

  ;; so magit buffers can be switched to (except for process buffers)
  (defun +magit-buffer-p (buffer)
    (with-current-buffer buffer
      (and (derived-mode-p 'magit-mode)
           (not (eq major-mode 'magit-process-mode)))))
  (add-to-list '+buffer-real-p-functions #'+magit-buffer-p nil #'eq))

(after! git-timemachine
  (setq git-timemachine-show-minibuffer-details t)
  (advice-add #'git-timemachine--show-minibuffer-details :override #'+git*update-header-line)
  (add-transient-hook! #'git-timemachine-blame (require 'magit-blame)))

(defun +git|enforce-commit-conventions ()
  "See https://chris.beams.io/posts/git-commit/"
  (setq fill-column 72
        git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line
                                             non-empty-second-line)))
(add-hook 'git-commit-mode-hook #'+git|enforce-commit-conventions)

(after! magit
  (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
  (pinentry-start))
