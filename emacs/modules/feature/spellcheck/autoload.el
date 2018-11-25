;;; feature/spellcheck/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 25 Nov 2018
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
(defun +spellcheck|immediately ()
  "Spellcheck the buffer when `flyspell-mode' is enabled."
  (when (and flyspell-mode +spellcheck-immediately)
    (flyspell-buffer)))
