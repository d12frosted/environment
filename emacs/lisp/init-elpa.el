;;; init-elpa.el --- Initialize ELPA -*- lexical-binding: t; -*-
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
;; Setup Emacs for installing packages from MELPA and Git
;; repositories. Enable configuration via `use-package'.
;;
;;; Code:

(require 'lib-plist)
(require 'init-path)

(defvar elpa-bootstrap-p nil)



(setq package-user-dir
      (expand-file-name
       "elpa/"
       path-packages-dir))


;; bootstrap straight.el

(setq-default
 straight-repository-branch "develop"
 straight-check-for-modifications nil
 straight-use-package-by-default t
 straight-base-dir path-packages-dir
 straight-profiles (list
                    (cons nil
                          (expand-file-name
                           "versions/default.el"
                           path-emacs-dir))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         path-packages-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/"
                 "raxod502/straight.el/"
                 "develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package

(setq-default
 use-package-enable-imenu-support t)
(straight-use-package 'use-package)



(use-package el-patch
  :straight t)


;; popular packages

(use-package s)
(use-package dash)
(use-package async)



(provide 'init-elpa)
;;; init-elpa.el ends here
