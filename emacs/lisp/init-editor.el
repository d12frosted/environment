;;; init-editor.el --- Editor configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
;; Emacs lacks only good editor. Try a little bit.
;;
;;; Code:



;; easier to search
(setq-default
 search-default-mode #'char-fold-to-regexp
 replace-char-fold t)

;; buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; this battle is simple
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)



;; electric everything (but there must be a way to disable it)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)
(defun editor-disable-electric-indent ()
  "Disable the command `electric-indent-mode' locally."
  (electric-indent-local-mode -1))
(defun editor-disable-electric-pair ()
  "Disable the command `electric-pair-mode' locally."
  (electric-pair-local-mode -1))


;; Whitespaces

(setq-default require-final-newline t)

(defun editor-show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'editor-show-trailing-whitespace))

;; Formatting
(setq-default
 ;; `ws-butler' is used for better whitespace handling
 delete-trailing-lines nil
 sentence-end-double-space nil
 word-wrap t)

(use-package ws-butler
  :ensure (:host github :repo "hlissner/ws-butler")
  :diminish
  :commands (ws-butler-global-mode)
  :init
  (ws-butler-global-mode)
  :config
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
                '(special-mode comint-mode term-mode eshell-mode))))


;; Disable backup files. While I find them useful in general, they
;; keep interfering with `org-roam'.

(setq make-backup-files nil)


;; auto-save files

(setq-default
 auto-save-list-file-prefix (expand-file-name
                             "auto-save-list/.saves-"
                             path-cache-dir))


;; Long lines

(setq-default fill-column 120)

(use-package visual-fill-column
  :ensure t
  :hook ((visual-line-mode . visual-fill-column-mode)))

(use-package adaptive-wrap
  :ensure t
  :defer t)

(use-package unfill
  :ensure t
  :commands (unfill-toggle)
  :bind
  (("M-q" . #'unfill-toggle)))



(use-package ukrainian-input-method
  :ensure (:host github :repo "d12frosted/emacs-ukrainian-input-method")
  :init
  (setq-default default-input-method "ukrainian"))



(use-package move-text
  :ensure t
  :commands (move-text-up
             move-text-down)
  :bind
  (([M-S-down] . #'move-text-down)
   ([M-S-up] . #'move-text-up)))



(use-package avy
  :ensure t
  :defer t
  :general
  (leader-def
    "jj" '(avy-goto-char :which-key "Char")
    "jl" '(avy-goto-line :which-key "Line (avy)")
    "jw" '(avy-goto-word-0 :which-key "Word")
    "jJ" '(avy-goto-char-timer :which-key "Chars")))

(use-package ace-link
  :ensure t
  :defer t
  :general
  (leader-def
    "jb" '(ace-link :which-key "Button or link")))



(use-package mwim
  :ensure t
  :defer t
  :bind (("C-a" . mwim-beginning)))



(provide 'init-editor)
;;; init-editor.el ends here
