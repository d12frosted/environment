;;; init-sh.el --- Shell support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Feb 2021
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
;; Shell support.
;;
;;; Code:

(require 'init-elpa)



(defvar sh-builtin-keywords
  '("cat" "cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find"
    "git" "grep" "kill" "less" "ls" "make" "mkdir" "mv" "pgrep"
    "pkill" "pwd" "rm" "sleep" "sudo" "touch")
  "A list of common commands to be fontified in `sh-mode'.")

(use-package sh-script
  :straight nil
  :commands (sh-shell-process)
  :config
  ;; setup indentation
  (setq sh-indent-after-continuation 'always
        sh-basic-offset tab-width)

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `sh-builtin-keywords')
  (font-lock-add-keywords
   'sh-mode
   `((sh--match-variables-in-quotes
      (1 'font-lock-constant-face prepend)
      (2 'font-lock-variable-name-face prepend))
     (sh--match-command-subst-in-quotes
      (1 'sh-quoted-exec prepend))
     (,(regexp-opt sh-builtin-keywords 'words)
      (0 'font-lock-type-face append)))))

(use-package company-shell
  :after sh-script
  :config
  (add-to-list 'company-backends '(company-shell
                                   company-shell-env
                                   company-fish-shell))
  (setq company-shell-delete-duplicates t))

(defun sh--match-variables-in-quotes (limit)
  "Search for variables in double-quoted strings.
Search is bounded by LIMIT."
  (with-syntax-table sh-mode-syntax-table
    (let (res)
      (while
          (and
           (setq
            res
            (re-search-forward
             "[^\\]\\(\\$\\)\\({.+?}\\|\\<[a-zA-Z0-9_]+\\|[@*#!]\\)"
             limit
             t))
           (not (eq (nth 3 (syntax-ppss)) ?\"))))
      res)))

(defun sh--match-command-subst-in-quotes (limit)
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

(defun sh-repl ()
  "Open a shell REPL."
  (let* ((dest-sh (symbol-name sh-shell))
         (sh-shell-file dest-sh)
         (dest-name (format "*shell [%s]*" dest-sh)))
    (sh-shell-process t)
    (with-current-buffer "*shell*"
      (rename-buffer dest-name))
    (get-buffer dest-name)))



(use-package fish-mode
  :defer t
  :init
  (setq-default fish-indent-offset tab-width))



(use-package eshell
  :straight nil
  :defer t
  :init
  (setq-default
   eshell-history-file-name (expand-file-name "eshell/history"
                                              path-cache-dir)))



(provide 'init-sh)
;;; init-sh.el ends here
