;;; init-env.el --- Environment configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
;; Defines constants for detecting the runtime environment: display type,
;; operating system, and user privileges. Used for conditional configuration.
;;
;;; Code:

(defconst env-graphic-p (display-graphic-p)
  "Non-nil if Emacs is running with a graphical display.")

(defconst env-rootp (string-equal "root" (getenv "USER"))
  "Non-nil if Emacs is running as root user.")

(defconst env-sys-mac-p (eq system-type 'darwin)
  "Non-nil if running on macOS.")

(defconst env-sys-linux-p (eq system-type 'gnu/linux)
  "Non-nil if running on GNU/Linux.")

(defconst env-sys-name (system-name)
  "The host name of the machine Emacs is running on.")

(provide 'init-env)
;;; init-env.el ends here
