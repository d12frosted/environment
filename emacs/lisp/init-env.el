;;; init-env.el --- Environment configurations -*- lexical-binding: t; -*-
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
;; Environment-related configurations.
;;
;;; Code:

(require 'init-elpa)

(defconst env-graphic-p (display-graphic-p))
(defconst env-rootp (string-equal "root" (getenv "USER")))
(defconst env-sys-mac-p (eq system-type 'darwin))
(defconst env-sys-linux-p (eq system-type 'gnu/linux))
(defconst env-sys-name (system-name))

(use-package exec-path-from-shell
  :if (and env-sys-mac-p env-graphic-p)
  :commands (exec-path-from-shell-initialize)
  :init
  (setq-default
   exec-path-from-shell-shell-name "/usr/local/bin/fish"
   exec-path-from-shell-debug nil)
  (exec-path-from-shell-initialize))

(provide 'init-env)
;;; init-env.el ends here
