;;; lang/sh/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 25 Nov 2018
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

(defvar +sh-builtin-keywords
  '("cat" "cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git"
    "grep" "kill" "less" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
    "sleep" "sudo" "touch")
  "A list of common commands to be fontified in `sh-mode'.")

(def-package! sh-script ; built-in
  :config
  ;; TODO electric
  ;; (set-electric! 'sh-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))

  (set-repl-handler! 'sh-mode #'+sh/repl)

  ;; setup indentation
  (setq sh-indent-after-continuation 'always
        sh-basic-offset tab-width)
  (setq-hook! 'sh-mode-hook +modeline-indent-width sh-basic-offset)

  ;; make the `mode-name' a little bit shorter
  (setq-hook! 'sh-mode-hook mode-name "sh")

  ;; recognize function names with dashes in them
  (add-to-list 'sh-imenu-generic-expression
               '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))

  ;; `sh-set-shell' is chatty about setting up indentation rules
  (advice-add #'sh-set-shell :around #'nucleus*shut-up)

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `+sh-builtin-keywords')
  (font-lock-add-keywords
   'sh-mode `((+sh--match-variables-in-quotes
               (1 'font-lock-constant-face prepend)
               (2 'font-lock-variable-name-face prepend))
              (+sh--match-command-subst-in-quotes
               (1 'sh-quoted-exec prepend))
              (,(regexp-opt +sh-builtin-keywords 'words)
               (0 'font-lock-type-face append))))
  ;; 4. Fontify delimiters by depth
  (add-hook 'sh-mode-hook #'rainbow-delimiters-mode)

  ;; autoclose backticks
  ;; TODO smart parens
  ;; (sp-local-pair 'sh-mode "`" nil :unless '(sp-point-before-word-p sp-point-before-same-p))
  )

(def-package! company-shell
  :after sh-script
  :config
  (set-company-backend! 'sh-mode '(company-shell company-files))
  (setq company-shell-delete-duplicates t))

(def-package! fish-mode
  :defer t
  :config
  ;; setup indentation
  (setq fish-indent-offset tab-width)
  (setq-hook! 'fish-mode-hook +modeline-indent-width fish-indent-offset)

  ;; TODO formatter
  ;;(set-formatter! 'fish-mode #'fish_indent)
  )
