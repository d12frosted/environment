;;; packages.el --- d12-core layer packages file for Spacemacs.
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

(defconst d12-core-packages
  '(
    (d12-macos-reveal :location (recipe :fetcher local)
                      :toggle (spacemacs/system-is-mac))
    (d12-dir-settings :location (recipe :fetcher local))
    ace-window
    projectile
    )
  "The list of Lisp packages required by the d12-core layer.")

(defun d12-core/init-d12-macos-reveal ()
  (use-package d12-macos-reveal
    :commands (d12-macos-reveal/file d12-macos-reveal/project-root)
    :init
    (spacemacs/declare-prefix "or" "reveal")
    (spacemacs/set-leader-keys
      "orf" #'d12-macos-reveal/file
      "orr" #'d12-macos-reveal/project-root)))

(defun d12-core/init-d12-dir-settings ()
  (use-package d12-dir-settings
    :defer t
    :init
    (add-hook 'find-file-hook #'d12-dir-settings/load)))

(defun d12-core/post-init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (bind-key "M-p" 'ace-window)))

(defun d12-core/pre-init-projectile ()
  (use-package projectile
    :defer t
    :init
    ;; Disable projectile caching as it slows down file switching in huge
    ;; projects. TODO: investigate if it's still the case.
    (setq projectile-enable-caching nil)))

;;; packages.el ends here
