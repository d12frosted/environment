;;; init-window.el --- Window configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Configurations for window system.
;;
;;; Code:

;; Prefer vertical splits by default
(setq split-height-threshold nil
      split-width-threshold 160)

(use-package ace-window
  :defer t
  :init
  (setq-default aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o)))

(defun window-split-vertically ()
  "Split window vertically."
  (interactive)
  (split-window-right))

(defun window-split-vertically-and-focus ()
  "Split window vertically and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun window-split-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-below))

(defun window-split-horizontally-and-focus ()
  "Split window horizontally and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun window-zoom ()
  "Close other windows to focus on this one.

Activate again to undo this. If the window changes before then,
the undo expires."
  (interactive)
  (if (and (one-window-p)
           (assq ?_ register-alist))
      (jump-to-register ?_)
    (window-configuration-to-register ?_)
    (delete-other-windows)))

(provide 'init-window)
;;; init-window.el ends here
