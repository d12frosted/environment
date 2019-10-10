;;; feature/dir-settings/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Jul 2019
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
(defvar +dir-settings-filename "settings.el"
  "Name of the file containing dir settings.")

;;;###autoload
(defvar-local +dir-settings-file nil
  "Path to the settings file of the current buffer.")

;;;###autoload
(defun +dir-settings-recursive-load (currentfile)
  "Find and load relevant settings file for CURRENTFILE."
  (let ((lds-dir (locate-dominating-file currentfile +dir-settings-filename)))
    (when lds-dir
      (setq +dir-settings-file (expand-file-name (concat lds-dir +dir-settings-filename)))
      (progn
        (load-file +dir-settings-file)
        (+dir-settings-recursive-load (file-truename (concat lds-dir "..")))))))

;;;###autoload
(defun +dir-settings/load ()
  "Load relevant settings file."
  (interactive)
  (when buffer-file-name
    (+dir-settings-recursive-load buffer-file-name)))
