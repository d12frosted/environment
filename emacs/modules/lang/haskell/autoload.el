;;; lang/haskell/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Dec 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +haskell-repl-buffer (arg)
  "Returns the appropriate Haskell REPL buffer."
  (if (featurep! +intero)
      (intero-repl-buffer arg)
    (haskell-session-interactive-buffer (haskell-session))))

;;;###autoload
(defun +haskell/repl (&optional arg)
  "Opens a Haskell REPL."
  (interactive "P")
  (display-buffer (+haskell-repl-buffer arg)))
