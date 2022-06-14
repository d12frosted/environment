;;; lib-calc.el --- Calc utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Apr 2022
;;
;; URL: https://github.com/d12frosted/
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
;; This module contains various utilities for working with `calc'
;; programatically.
;;
;; Feedback on this code is highly appreciated!
;;
;;; Code:

(require 'calc)

(defun calc-from-number (number)
  "Convert NUMBER to Calc format."
  (math-read-number (number-to-string number)))

(defun calc-to-number (number)
  "Convert NUMBER from Calc format."
  (read (math-format-number number)))

(provide 'lib-calc)
;;; lib-calc.el ends here
