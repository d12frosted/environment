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

(require '+keybindings)

(use-package general)

(global-set-key [remap keyboard-quit] #'+escape)

(general-create-definer +leader-def
  :states nil
  :keymaps 'override
  :prefix "M-m"
  :prefix-command '+prefix-command
  :prefix-map '+prefix-map)

(+leader-def
  "a" '(nil :which-key "align...")
  "c" '(nil :which-key "capture...")
  "j" '(nil :which-key "jump...")
  "o" '(nil :which-key "open...")
  "i" '(nil :which-key "insert...")
  "[" '(nil :which-key "previous...")
  "/" '(nil :which-key "search..."))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(when (and +sys-mac-p +sys-graphic-p)
  (setq mac-option-modifier nil
	mac-command-modifier 'meta))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
