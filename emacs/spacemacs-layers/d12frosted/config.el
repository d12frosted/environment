;;; config.el --- d12frosted Layer config File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; omnisharp
;; configurations specific to c# and omnisharp

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

;;; js
;; configurations specifc to js

(setq js-indent-level 2)

;;; text-mode
;; configurations specific to text-mode and all modes derived from text-mode

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'turn-off-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

;;; random stuff

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq projectile-globally-ignored-file-extensions
      '(".DS_Store"))
