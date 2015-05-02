;;; extensions.el --- d12frosted Layer extensions File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-pre-extensions
  '(fish-mode)
  "List of all extensions to load before the packages.")

(defvar d12frosted-post-extensions
  '()
  "List of all extensions to load after the packages.")

(defun d12frosted/init-fish-mode ()
  (use-package fish-mode
    :defer 1
    :init
    :config))
