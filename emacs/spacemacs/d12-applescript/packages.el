;;; packages.el --- d12-applescript layer packages file for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst d12-applescript-packages
  '((applescript :location built-in)
    apples-mode)
  "The list of Lisp packages required by the d12-applescript layer.")

(defun d12-applescript/init-applescript ()
  ;; Allow editing of binary .scpt files (applescript) on mac.
  (add-to-list 'jka-compr-compression-info-list
               `["\\.scpt\\'"
                 "converting text applescript to binary applescprit "
                 ,(executable-find "applescript-helper") nil
                 "converting binary applescript to text applescprit "
                 ,(executable-find "applescript-helper") ("-d")
                 nil t "FasdUAS"])
  ;; It is necessary to perform an update!
  (jka-compr-update))


(defun d12-applescript/init-apples-mode ()
  (use-package apples-mode
    ;; TODO: fix auto-mode that is broken due to usage of jka-compr
    :mode "\\.scpt\\'"
    :init
    (setq apples-indent-offset 2)))

;;; packages.el ends here
