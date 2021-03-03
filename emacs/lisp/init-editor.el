;;; init-editor.el --- Editor configuration -*- lexical-binding: t; -*-
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
;; Emacs lacks only good editor. Try a little bit.
;;
;;; Code:

(require 'init-elpa)
(require 'init-kbd)
(require 'lib-vcs)
(require 'lib-string)



;; easier to search
(setq-default
 search-default-mode #'char-fold-to-regexp
 replace-char-fold t)



;; electric everything (but there must be a way to disable it)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)
(defun editor-disable-electric-indent ()
  "Disable the command `electric-indent-mode' locally."
  (electric-indent-local-mode -1))


;; Whitespaces

(setq-default
 indent-tabs-mode nil
 tab-width 2
 require-final-newline t
 tab-always-indent t)

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
  :straight (ws-butler
             :type git
             :host github
             :repo "hlissner/ws-butler")
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



(use-package unfill
  :defer t
  :bind
  (("M-q" . #'unfill-toggle)))



(use-package ukrainian-input-method
  :straight (ukrainian-input-method
             :type git
             :host github
             :repo "d12frosted/emacs-ukrainian-input-method"))



(use-package move-text
  :bind
  (([M-S-down] . #'move-text-down)
   ([M-S-up] . #'move-text-up)))



(use-package fancy-yank
  :straight (fancy-yank
             :type git
             :host github
             :repo "d12frosted/fancy-yank")
  :bind
  (("C-S-y" . #'fancy-yank))
  :init
  (setq-default
   fancy-yank-rules
   (list
    (cons vcs-url-github-issue-regexp
          '(fancy-yank-extract-regex
            (lambda (url owner repo type number &rest args)
              (list url
                    (vcs-url-format-github-issue
                     owner repo type number)))
            fancy-yank-format-link))
    (cons vcs-url-github-project-regexp
          '(fancy-yank-extract-regex
            (lambda (url owner repo &rest args)
              (list url
                    (vcs-url-format-github-project owner repo)))
            fancy-yank-format-link))
    (cons (format "\\(https?://%s/package/\\([-[:alnum:]]+\\).*\\)"
                  "hackage.haskell.org")
          '(fancy-yank-extract-regex
            (lambda (url package &rest args)
              (list
               url
               package))
            fancy-yank-format-link))
    (cons string-http-url-regexp
          '(fancy-yank-extract-regex
            (lambda (url &rest args)
              (list
               url
               (or (ignore-errors (url-domain
                                   (url-generic-parse-url url)))
                   (read-string "Description: "))))
            fancy-yank-format-link)))))



(use-package avy
  :defer t
  :general
  (leader-def
    "jj" '(avy-goto-char :which-key "Char")
    "jl" '(avy-goto-line :which-key "Line (avy)")
    "jw" '(avy-goto-word-0 :which-key "Word")
    "jJ" '(avy-goto-char-timer :which-key "Chars")))

(use-package ace-link
  :defer t
  :general
  (leader-def
    "jb" '(ace-link :which-key "Button or link")))



(use-package mwim
  :defer t
  :bind (("C-a" . mwim-beginning)))



(provide 'init-editor)
;;; init-editor.el ends here
