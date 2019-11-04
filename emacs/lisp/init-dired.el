;;; init-dired.el --- dired configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Nov 2019
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

(eval-when-compile
  (require 'dired))

(setq
 dired-listing-switches "-alh"
 dired-recursive-copies  'always
 dired-recursive-deletes 'top
 dired-auto-revert-buffer t
 dired-hide-details-hide-symlink-targets nil)

;; sort dired buffer so directories are first
(add-hook 'dired-after-readin-hook #'+dired|sort-directories-first)
(defun +dired|sort-directories-first ()
  "List directories first in dired buffers."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (set-buffer-modified-p nil))

(use-package dired-k
  :hook ((dired-initial-position . dired-k)
         (dired-after-readin . dired-k-no-revert))
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

(provide 'init-dired)
;;; init-dired.el ends here
