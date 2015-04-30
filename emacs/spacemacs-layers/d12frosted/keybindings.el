;;; keybindings.el --- d12frosted Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;; Window manipulations

;; (global-set-key (kbd "<left>") 'evil-window-left)
;; (global-set-key (kbd "<right>") 'evil-window-right)
;; (global-set-key (kbd "<up>") 'evil-window-up)
;; (global-set-key (kbd "<down>") 'evil-window-down)

;; (global-set-key (kbd "A-<left>") 'evil-window-move-far-left)
;; (global-set-key (kbd "A-<right>") 'evil-window-move-far-right)
;; (global-set-key (kbd "A-<up>") 'evil-window-move-very-top)
;; (global-set-key (kbd "A-<down>") 'evil-window-move-very-down)

;; text manipulations

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "M-…") (kbd "C-a M-; C-n"))

(global-set-key (kbd "C-w") 'cut-line-or-region)
(global-set-key (kbd "M-w") 'copy-line-or-region)
(global-set-key (kbd "C-y") 'yank)
(global-set-key (kbd "C-S-w") 'duplicate-line-or-region)
(global-set-key (kbd "M-K") 'delete-line-or-region)

(global-set-key (kbd "M-g g") 'd12/goto-line-and-center)
