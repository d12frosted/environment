;;; packages.el --- d12frosted-lua layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-lua-packages
  '(lua-mode
    smart-ops
    window-purpose))

(defun d12frosted-lua/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'lua-mode "sm" 'moai-run-main)
    (spacemacs/set-leader-keys-for-major-mode 'lua-mode "su" 'moai-upload)))

(defun d12frosted-lua/post-init-smart-ops ()
  (use-package smart-ops
    :after lua-mode
    :config
    (define-smart-ops-for-mode 'lua-mode
      (smart-ops ":" :pad-before nil :pad-after nil)
      (smart-ops "," :pad-before nil)
      (smart-ops "~=" ">=" "<=" "|")
      (smart-ops-default-ops))))

(defun d12frosted-lua/post-init-window-purpose ()
  (use-package window-purpose
    :config
    (add-to-list 'purpose-user-mode-purposes '(lua-mode . edit))
    (add-to-list 'purpose-user-mode-purposes '(json-mode . edit))
    (add-to-list 'purpose-user-mode-purposes '(shell-mode . edit))
    (add-to-list 'purpose-user-name-purposes (cons d12frosted-lua-moai-buffer-name 'terminal))
    (purpose-compile-user-configuration)))

;;; packages.el ends here
