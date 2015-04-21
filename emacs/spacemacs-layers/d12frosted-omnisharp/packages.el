;;; packages.el --- d12frosted-omnisharp Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-omnisharp-packages
  '(omnisharp
    csharp-mode
    company)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-omnisharp-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted-omnisharp/init-company ()
  "Initialize org-mode package."
  (use-package company
    :defer 1
    :init
    :config
    ))

(defun d12frosted-omnisharp/init-csharp-mode ()
  "Initialize csharp-mode package."
  (use-package csharp-mode
    :defer 2
    :init
    :config
    ))

(defun d12frosted-omnisharp/init-omnisharp ()
  "Initialize omnisharp package."
  (use-package omnisharp
    :defer 3
    :init
    :config

    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-hook 'csharp-mode-hook 'company-mode)
    (add-hook 'csharp-mode-hook 'd12frosted/omnisharp-on-load-fn t)

    (setq omnisharp-server-executable-path "~/.omnisharp/OmniSharp/bin/Debug/OmniSharp.exe")

    (unless (assoc 'csharp-mode hs-special-modes-alist)
      (push '(csharp-mode
              ;; regexp for start block
              "\\(^[ \\t]*#[ \\t]*region\\b\\)\\|{"

              ;; regexp for end block
              "\\(^[ \\t]*#[ \\t]*endregion\\b\\)\\|}"

              ;; regexp for comment start
              "/[*/]"

              ;; hs-forward-sexp-func
              csharp-hs-forward-sexp

              ;; c-like adjust (1 char)
              hs-c-like-adjust-block-beginning)
            hs-special-modes-alist))

    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-omnisharp))

    ))
