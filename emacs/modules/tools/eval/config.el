;;; feature/eval/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Dec 2018
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

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(after! quickrun
  (setq quickrun-focus-p nil)

  ;; TODO popup
  ;; (set-popup-rule! "^\\*quickrun" :size 0.3 :ttl 0)

  (defun +eval*quickrun-auto-close (&rest _)
    "Allows us to silently re-run quickrun from within the quickrun buffer."
    (when-let* ((win (get-buffer-window quickrun--buffer-name)))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))
  (advice-add #'quickrun :before #'+eval*quickrun-auto-close)
  (advice-add #'quickrun-region :before #'+eval*quickrun-auto-close)

  (defun +eval|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (with-selected-window (get-buffer-window quickrun--buffer-name)
      (goto-char (point-min))
      (let ((ignore-window-parameters t))
        (shrink-window-if-larger-than-buffer))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-scroll-to-bof))
