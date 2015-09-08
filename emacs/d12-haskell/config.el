;;; config.el --- d12-haskell Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Boris Buliga & Contributors
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar haskell-enable-shm-support nil
  "If non-nil structured-haskell-mode support is enabled")

(defvar haskell-enable-hindent-style nil
  "Style to use for formatting with hindent; available are: fundamental johan-tibell chris-done gibiansky. If nil hindent is disabled.")

(spacemacs|defvar-company-backends haskell-mode)
(spacemacs|defvar-company-backends haskell-interactive-mode)
