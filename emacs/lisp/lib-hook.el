;;; lib-hook.el --- hooking into hooks  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 22 Jul 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'lib-list)
(require 'lib-fun)

(defalias '+hook #'add-hook)

(defmacro +hook-with-delay (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.

The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.

DEPTH and LOCAL are passed as is to `+hook'."
  (let* ((f-name-str (concat (symbol-name (+unquote function)) "-with-delay"))
         (f-name (make-symbol f-name-str))
         (doc (format "Call `%s' in %s seconds"
                      (symbol-name (+unquote function))
                      secs)))
    `(progn
       (defun ,f-name () ,doc
              (run-with-idle-timer ,secs nil ,function))
       (+hook ,hook #',f-name ,depth ,local))))

(provide 'lib-hook)
;;; lib-hook.el ends here
