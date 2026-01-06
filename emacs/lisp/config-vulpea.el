;;; config-vulpea.el --- Vulpea configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 14 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
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
;; Configuration variables for vulpea, a personal knowledge management
;; system built on org-roam. Defines the notes directory location and
;; test mode toggle.
;;
;;; Code:

(require 'config-path)

(defvar vulpea-test-mode
  (file-exists-p
   (expand-file-name "vulpea_test" path-cache-dir))
  "Non-nil if notes should start in a test mode.

Probably that means using directory with test notes instead of
real notes. Maybe it also means experimental features.")

(defvar vulpea-directory
  (if vulpea-test-mode
      (expand-file-name
       "vulpea-test/"
       path-home-dir)
    (expand-file-name
       "vulpea/"
       path-home-dir))
  "Directory containing notes.")

(provide 'config-vulpea)
;;; config-vulpea.el ends here
