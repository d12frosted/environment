;;; packages.el --- d12frosted-omnisharp Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-omnisharp-packages
  '(
    company
    csharp-mode
    omnisharp)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-omnisharp-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted-omnisharp/init-omnisharp ()
  "Initialize omnisharp package"
  (use-package omnisharp
    :defer t

    :init
    (setq omnisharp-server-executable-path "~/.omnisharp/OmniSharp/bin/Debug/OmniSharp.exe")

    :config
    (progn
      (spacemacs|diminish omnisharp-mode " ⓞ" " o"))))
