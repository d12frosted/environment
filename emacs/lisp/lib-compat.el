;;; lib-compat.el --- Compatibility utilities -*- lexical-binding: t; -*-
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
;; While these configurations are aimed for bleeding edge of Emacs,
;; compatibility with latest released version of Emacs is possible.
;; This module defines missing bits.
;;
;;; Code:

(when (> emacs-major-version 28)
  (error "Redundant compat function: dlet"))

;;;###autoload
(defmacro dlet (binders &rest body)
  "Like `let*' but using dynamic scoping.

Dynamically bind the BINDERS and evaluate the BODY."
  (declare (indent 1) (debug let))
  ;; (defvar FOO) only affects the current scope, but in order for
  ;; this not to affect code after the `let*' we need to create a new
  ;; scope, which is what the surrounding `let' is for.
  ;; FIXME: (let () ...) currently doesn't actually create a new
  ;; scope, which is why we use (let (_) ...).
  `(let (_)
     ,@(mapcar (lambda (binder)
                 `(defvar ,(if (consp binder) (car binder) binder)))
               binders)
     (let* ,binders ,@body)))

(provide 'lib-compat)
;;; lib-compat.el ends here
