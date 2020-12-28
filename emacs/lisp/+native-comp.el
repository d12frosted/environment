;;; +native-comp.el --- native compilation helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 28 Dec 2020
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

(require 'comp)

(defun +native-compile (dir)
  "Natively compile files in DIR."
  (add-hook 'comp-async-cu-done-hook
            (lambda (f)
              (message "done compiling %s" f)))
  (native-compile-async dir t)
  (while comp-files-queue
    (message "compiling %s files..." (length comp-files-queue))
    (sleep-for 2)))

(provide '+native-comp)
;;; +native-comp.el ends here
