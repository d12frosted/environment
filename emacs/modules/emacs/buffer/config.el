;;; emacs/buffer/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Nov 2018
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

;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
(add-to-list 'default-frame-alist '(buffer-predicate . +buffer-frame-predicate))

(defun +buffer|protect-visible-buffer ()
  "Don't kill the current buffer if it is visible in another
window (bury it instead).

Meant for `kill-buffer-query-functions'."
  (not (and (delq (selected-window) (get-buffer-window-list nil nil t))
            (not (member (substring (buffer-name) 0 1) '(" " "*"))))))

(defun +buffer|protect-fallback-buffer ()
  "Don't kill the scratch buffer.

Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (+buffer-fallback))))

(add-to-list 'kill-buffer-query-functions #'+buffer|protect-fallback-buffer nil #'eq)
(add-to-list 'kill-buffer-query-functions #'+buffer|protect-visible-buffer  nil #'eq)
