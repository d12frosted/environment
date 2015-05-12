 ;;; lisp-configs.el --- configs file of lisp configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 11 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

    ;; flash !
(use-package eval-sexp-fu
  :ensure t)

 ;; Elisp go-to-definition with M-. and back again with M-,
(use-package elisp-slime-nav
  :ensure t
  :init
  (d12|lazy-diminish elisp-slime-nav-mode "")
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  ;; the defaul is <C-c C-d d>
  (bind-keys
   :map emacs-lisp-mode-map
   ("C-c c d" . elisp-slime-nav-describe-elisp-thing-at-point)))

;; just to try
(use-package paredit
  :ensure t
  :init
  ;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  :config
  (bind-keys
   :map paredit-mode-map
   ("M-k" . d12/paredit-kill-sexp)))

(bind-keys
 :map emacs-lisp-mode-map
 ("C-x C-e" . d12/eval-last-sexp-or-region))
