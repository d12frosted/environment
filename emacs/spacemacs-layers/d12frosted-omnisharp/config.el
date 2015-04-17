;;; config.el --- d12frosted-omnisharp Layer config File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'company-mode)
(add-hook 'csharp-mode-hook 'omnisharp/on-load-fn t)

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
