;;; packages.el --- vulpea layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst vulpea-packages
  '((vulpea :location built-in))
  "The list of Lisp packages required by the vulpea layer.")

(defun vulpea/init-vulpea ()
  (add-to-load-path-if-exists (concat d12-path-spacemacs-config-home "vulpea/local/vulpea/"))
  (use-package vulpea
    :commands (vulpea-mode-enable)
    :init
    (add-hook 'org-mode-hook #'vulpea-mode-enable)
    (setq vulpea-cha-tea-groups-parent-id "A023CFA4-E68B-48E5-BF97-AFA34936F57A")
    (setq vulpea-cha-tea-parent-id "F01A4D43-F79E-4640-B98A-9A7E07B86773")))

;;; packages.el ends here
