;;; extensions.el --- d12frosted-haskell Layer extensions File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-haskell-pre-extensions
  '(hindent/elisp
    structured-haskell-mode/elisp)
  "List of all extensions to load before the packages.")

(defvar d12frosted-haskell-post-extensions
  '()
  "List of all extensions to load after the packages.")

(defun d12frosted-haskell/init-hindent/elisp ()
  "Initialize hindent extension."
  (use-package hindent
    :defer t
    :init
    :config))

(defun d12frosted-haskell/init-structured-haskell-mode/elisp ()
  "Initialize structured-haskell-mode extension."
  (use-package structured-haskell-mode
    :defer t
    :init
    :config))
