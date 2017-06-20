;;; packages.el --- d12-visual layer packages file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2017 Boris Buliga
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

(defconst d12-visual-packages
  '(spaceline-all-the-icons)
  "The list of Lisp packages required by the d12-visual layer.")

(defun d12-visual/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :after spaceline
    :init
    (setq spaceline-all-the-icons-separator-type 'none)
    (setq spaceline-all-the-icons-icon-set-flycheck-slim 'dots)
    (setq spaceline-all-the-icons-icon-set-git-ahead 'commit)
    ;; (setq spaceline-all-the-icons-icon-set-window-numbering 'square)
    (setq spaceline-all-the-icons-flycheck-alternate t)
    (setq spaceline-all-the-icons-highlight-file-name t)
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-anzu)
    (spaceline-all-the-icons--setup-package-updates)
    (spaceline-all-the-icons--setup-paradox)
    (spaceline-all-the-icons--setup-neotree)
    (spaceline-toggle-all-the-icons-buffer-path-off)
    (spaceline-toggle-all-the-icons-fullscreen-on)
    (spaceline-toggle-all-the-icons-flycheck-status-on)
    (spaceline-toggle-all-the-icons-vc-status-on)
    (spaceline-toggle-all-the-icons-git-status-on)
    (spaceline-toggle-all-the-icons-vc-icon-on)
    (spaceline-toggle-all-the-icons-mode-icon-on)
    (spaceline-toggle-all-the-icons-package-updates-on)
    (spaceline-toggle-all-the-icons-window-number-off)
    (spaceline-toggle-all-the-icons-projectile-off)
    (spaceline-toggle-all-the-icons-text-scale-off)
    (spaceline-toggle-all-the-icons-region-info-on)
    (spaceline-toggle-all-the-icons-battery-status-off)
    (spaceline-toggle-all-the-icons-time-off)
    :config (spaceline-all-the-icons-theme)))

;;; packages.el ends here
