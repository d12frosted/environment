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
  '(
    ;; pre extension d12frosteds go here
    )
  "List of all extensions to load before the packages.")

(defvar d12frosted-post-extensions
  '(
    ;; post extension d12frosteds go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function d12frosted/init-<extension-d12frosted>
;;
;; (defun d12frosted/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
