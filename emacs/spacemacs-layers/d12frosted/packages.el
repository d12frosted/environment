;;; packages.el --- d12frosted Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-packages
  '(
    ;; package d12frosteds go here
    helm-spotify
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted/init-helm-spotify ()
  "Initialize helm-spotify package."
  (use-package helm-spotify
    :defer t
    :init
    :config))

;; For each package, define a function d12frosted/init-<package-d12frosted>
;;
;; (defun d12frosted/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
