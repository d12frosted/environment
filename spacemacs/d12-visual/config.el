;;; config.el --- d12-visual layer config file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defface d12-flycheck-mode-line
  '()
  "Face for flycheck status in mode line."
  :group 'custom-faces)

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "  "
   "("
   mode-name
   mode-line-process
   ")"
   (vc-mode vc-mode)
   " "
   (:eval
    (propertize (flycheck-mode-line-status-text) 'face 'd12-flycheck-mode-line))
   " "
   mode-line-position
   mode-line-end-spaces))

(add-hook 'window-setup-hook 'd12-patch-background)

;;; config.el ends here
