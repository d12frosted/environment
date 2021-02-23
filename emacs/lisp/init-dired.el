;;; init-dired.el --- Dired configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021
;;
;; Author:  <d12frosted@borysb-arch>
;; Maintainer:  <d12frosted@borysb-arch>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 16 Feb 2021
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
;; Configurations for wonderful `dired'.
;;
;;; Code:

(require 'init-elpa)



(use-package dired
  :straight nil
  :init
  (setq
   dired-listing-switches "-alh"
   dired-recursive-copies 'always
   dired-recursive-deletes 'top
   dired-auto-revert-buffer t
   dired-hide-details-hide-symlink-targets nil))



;; sort dired buffer so directories are first
(add-hook 'dired-after-readin-hook #'dired-sort-directories-first)

(defun dired-sort-directories-first ()
  "List directories first in dired buffers."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (set-buffer-modified-p nil))



(provide 'init-dired)
;;; init-dired.el ends here
