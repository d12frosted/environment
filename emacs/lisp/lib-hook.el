;;; lib-hook.el --- Hook utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
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
;; Hook utilities: `hook-with-delay' adds a function to a hook that
;; runs after an idle delay instead of immediately.
;;
;;; Code:

(require 'lib-fun)

(defmacro hook-with-delay (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.

The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.

DEPTH and LOCAL are passed as is to `add-hook'."
  (let* ((f-name-str (concat (symbol-name (fun-unquote function))
                             "-"
                             (symbol-name (fun-unquote hook))
                             "-with-delay"))
         (f-name (make-symbol f-name-str))
         (doc (format "Call `%s' in %s seconds"
                      (symbol-name (fun-unquote function))
                      secs)))
    `(progn
       (eval-when-compile
         (defun ,f-name () ,doc
                (run-with-idle-timer ,secs nil ,function))
         (add-hook ,hook #',f-name ,depth ,local)))))

(provide 'lib-hook)
;;; lib-hook.el ends here
