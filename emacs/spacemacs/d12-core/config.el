;;; config.el --- d12-core layer config file for Spacemacs. -*- lexical-binding: t; -*-
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

;; Setup scrolling for trackpad
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Delete selection mode is my friend. Sorry, no-lifers!
(delete-selection-mode 1)

;; Aggressive auto-save
(setq auto-save-timeout 1)
(setq auto-save-interval 30)
(add-hook 'focus-out-hook #'d12-save-all)

;;; config.el ends here
