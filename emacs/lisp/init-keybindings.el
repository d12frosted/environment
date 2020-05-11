;;; init-keybindings.el --- keybindings -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
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

(require 'init-env)

(use-package general
  :commands (general-define-key
             general-create-definer))

(general-create-definer +leader-def
  :states nil
  :keymaps 'override
  :prefix "M-m"
  :prefix-command '+prefix-command
  :prefix-map '+prefix-map)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;;###autoload
(defvar +escape-hook nil
  "A hook run after \\[keyboard-quit] is pressed.

Triggers `+escape'.

If any hook returns non-nil, all hooks after it are ignored.")

;;;###autoload
(defun +escape ()
  "Run the `+escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((cl-find-if #'funcall +escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'+escape)

(when (and +sys-mac-p +sys-graphic-p)
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
