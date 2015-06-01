;;; python-configs.el --- configs file of python configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 27 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT
;;
;;; Comments
;; Dependencies
;; pip install  jedi==0.8.1 json-rpc==1.8.1 service_factory==0.1.2

;;; Code

;; (spacemacs|defvar-company-backends python-mode)

(use-package eldoc
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package python
  :ensure t
  :defer t
  :init
  (add-all-to-hook 'python-mode-hook
                   'python-default
                   'python-setup-shell)
  :config
  (add-hook 'inferior-python-mode-hook 'smartparens-mode)
  ;; add support for `ahs-range-beginning-of-defun' for python-mode
  (eval-after-load 'auto-highlight-symbol
    '(add-to-list 'ahs-plugin-bod-modes 'python-mode))

  ;; reset compile-command (by default it is `make -k')
  (setq compile-command nil)

  (bind-keys
   :mode python-mode-map
   ("C-c c c" . spacemacs/python-execute-file)
   ("C-c c C" . spacemacs/python-execute-file-focus)
   ("C-c c d b" . python-toggle-breakpoint)
   ("C-c c s B" . python-shell-send-buffer-switch)
   ("C-c c s b" . python-shell-send-buffer)
   ("C-c c s F" . python-shell-send-defun-switch)
   ("C-c c s f" . python-shell-send-defun)
   ("C-c c s i" . python-start-or-switch-repl)
   ("C-c c s R" . python-shell-send-region-switch)
   ("C-c c s r" . python-shell-send-region))

  (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "C-l") 'comint-clear-buffer)
  (define-key inferior-python-mode-map (kbd "C-r") 'comint-history-isearch-backward))

(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'company-mode)
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-anaconda
  :ensure t
  :defer t)
