;;; init-sh.el --- shell configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Nov 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'sh-script))

(defvar +sh-builtin-keywords
  '("cat" "cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git"
    "grep" "kill" "less" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common commands to be fontified in `sh-mode'.")

;; built-in
(use-package sh-script
  :commands (sh-shell-process)
  :config
  ;; setup indentation
  (setq sh-indent-after-continuation 'always
        sh-basic-offset tab-width)

  ;; recognize function names with dashes in them
  (add-to-list 'sh-imenu-generic-expression
               '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `+sh-builtin-keywords')
  (font-lock-add-keywords
   'sh-mode
   `((+sh--match-variables-in-quotes
      (1 'font-lock-constant-face prepend)
      (2 'font-lock-variable-name-face prepend))
     (+sh--match-command-subst-in-quotes
      (1 'sh-quoted-exec prepend))
     (,(regexp-opt +sh-builtin-keywords 'words)
      (0 'font-lock-type-face append))))
  ;; 4. Fontify delimiters by depth
  (add-hook 'sh-mode-hook #'rainbow-delimiters-mode))

(use-package company-shell
  :after sh-script
  :config
  (+company-set-backend 'sh-mode '(company-shell company-files))
  (setq company-shell-delete-duplicates t))

(use-package fish-mode
  :defer t
  :config
  ;; setup indentation
  (setq fish-indent-offset tab-width))

;;;###autoload
(defun +sh--match-variables-in-quotes (limit)
  "Search for variables in double-quoted strings.

Search is bounded by LIMIT."
  (with-syntax-table sh-mode-syntax-table
    (let (res)
      (while
          (and (setq res
                     (re-search-forward
                      "[^\\]\\(\\$\\)\\({.+?}\\|\\<[a-zA-Z0-9_]+\\|[@*#!]\\)"
                      limit
                      t))
               (not (eq (nth 3 (syntax-ppss)) ?\"))))
      res)))

;;;###autoload
(defun +sh--match-command-subst-in-quotes (limit)
  "Search for variables in double-quoted strings.

Search is bounded by LIMIT."
  (with-syntax-table sh-mode-syntax-table
    (let (res)
      (while
          (and (setq res
                     (re-search-forward
                      "[^\\]\\(\\$(.+?)\\|`.+?`\\)"
                      limit
                      t))
               (not (eq (nth 3 (syntax-ppss)) ?\"))))
      res)))

;;;###autoload
(defun +sh/repl ()
  "Open a shell REPL."
  (let* ((dest-sh (symbol-name sh-shell))
         (sh-shell-file dest-sh)
         (dest-name (format "*shell [%s]*" dest-sh)))
    (sh-shell-process t)
    (with-current-buffer "*shell*"
      (rename-buffer dest-name))
    (get-buffer dest-name)))

(provide 'init-sh)
;;; init-sh.el ends here
