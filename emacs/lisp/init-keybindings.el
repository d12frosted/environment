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

(require 'lib-keybindings)

(use-package general)

(global-set-key [remap keyboard-quit] #'+escape)

(general-create-definer +leader-def
  :states nil
  :keymaps 'override
  :prefix "M-m"
  :prefix-command 'nucleus-prefix-command
  :prefix-map 'nucleus-prefix-map)

(+leader-def
  "a" '(nil :which-key "align...")
  "o" '(nil :which-key "open...")
  "c" '(nil :which-key "capture...")
  "[" '(nil :which-key "previous..."))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
