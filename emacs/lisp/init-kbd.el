;;; init-kbd.el --- Keybinding configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Install all required packages and setup key bindings.
;;
;;; Code:

(require 'init-elpa)
(require 'init-env)

(use-package general)

(general-create-definer leader-def
  :states nil
  :keymaps 'override
  :prefix "M-m"
  :prefix-command 'prefix-command
  :prefix-map 'prefix-map)

(leader-def
  "/" '(nil :which-key "search...")
  "[" '(nil :which-key "previous...")
  "a" '(nil :which-key "align...")
  "g" '(nil :which-key "git...")
  "i" '(nil :which-key "insert...")
  "j" '(nil :which-key "jump...")
  "o" '(nil :which-key "open...")
  "v" '(nil :which-key "vino..."))

(use-package bind-key)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(defvar kbd-escape-hook nil
  "A hook run after \\[keyboard-quit] is pressed.

Triggers `kbd-escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun kbd-escape ()
  "Run the `kbd-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop
        ;; there.
        ((cl-find-if #'funcall kbd-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'kbd-escape)

(when (and env-sys-mac-p env-graphic-p)
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))

(provide 'init-kbd)
;;; init-kbd.el ends here
