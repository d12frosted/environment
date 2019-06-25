;;; emacs/dired/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 13 Dec 2018
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

(def-package! dired
  :defer t
  :init
  ;; visit file or directory in place, reuse buffers
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
   dired-listing-switches "-alh"
   ;; Always copy/delete recursively
   dired-recursive-copies  'always
   dired-recursive-deletes 'top
   ;; Auto refresh dired, but be quiet about it
   dired-auto-revert-buffer t
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil
   dired-hide-details-hide-symlink-targets nil)
  :config
  ;; Kill buffer when quitting dired buffers
  (define-key dired-mode-map [remap quit-window] (λ! (quit-window t)))
  ;; reuse buffers when navigating
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  ;; sort dired buffer so directories are first
  (defun +dired|sort-directories-first ()
    "List directories first in dired buffers."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook #'+dired|sort-directories-first))

(def-package! dired-k
  :hook (dired-initial-position . dired-k)
  :hook (dired-after-readin . dired-k-no-revert)
  :config
  (setq dired-k-human-readable t)
  (defun +dired*interrupt-process (orig-fn &rest args)
    "Fixes dired-k killing git processes too abruptly, leaving
behind disruptive .git/index.lock files."
    (cl-letf (((symbol-function #'kill-process)
               (symbol-function #'interrupt-process)))
      (apply orig-fn args)))
  (advice-add #'dired-k--start-git-status :around #'+dired*interrupt-process)

  (defun +dired*dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through
tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired*dired-k-highlight))
