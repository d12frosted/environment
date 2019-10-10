;;; feature/file-templates/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;;         Henrik Lissner <henrik@lissner.net>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

(defvar +file-templates-dir
  (expand-file-name "templates/" nucleus-emacs-dir)
  "The path to a directory of yasnippet folders to use for file templates.")

(defvar +file-templates-default-trigger "__"
  "The default yasnippet trigger key (a string) for file template
rules that don't have a :trigger property in
`+file-templates-alist'.")

(defvar +file-templates-alist
  `(;; elisp
    ("/.dir-locals.el$")
    ("\\.el$"
     :when +file-templates-in-nucleus-dirs-p
     :trigger "__nucleus"
     :mode emacs-lisp-mode)
    ("/packages\\.el$"
     :when +file-templates-in-emacs-dirs-p
     :trigger "__module-packages"
     :mode emacs-lisp-mode)
    ("\\.el$"
     :when +file-templates-in-emacs-dirs-p
     :trigger "__module-config"
     :mode emacs-lisp-mode)
    (emacs-lisp-mode
     :trigger "__package")
    (snippet-mode))
  "An alist of file template rules. The CAR of each rule is
either a major mode symbol or regexp string. The CDR is a
plist. See `set-file-template!' for more information.")

(defun +file-templates-in-emacs-dirs-p (file)
  "Returns t if FILE is in `nucleus-emacs-dir'."
  (file-in-directory-p file nucleus-emacs-dir))

(defun +file-templates-in-nucleus-dirs-p (file)
  "Return t if FILE ins in `nucleus-dir'."
  (file-in-directory-p file nucleus-dir))

(defun +file-templates|check ()
  "Check if the current buffer is a candidate for file template
expansion. It must be non-read-only, empty, and there must be a
rule in `+file-templates-alist' that applies to it."
  (when (and (not buffer-read-only)
             (bobp) (eobp)
             (not (string-match-p "^ *\\*" (buffer-name))))
    (when-let* ((rule (cl-find-if #'+file-template-p +file-templates-alist)))
      (apply #'+file-templates--expand rule))))

(after! yasnippet
  (if (featurep! :feature snippets)
      (add-to-list 'yas-snippet-dirs '+file-templates-dir 'append #'eq)
    (setq yas-prompt-functions (delq #'yas-dropdown-prompt yas-prompt-functions)
          yas-snippet-dirs '(+file-templates-dir))
    ;; Ensure file templates in `+file-templates-dir' are visible
    (yas-reload-all)))

(add-hook 'find-file-hook #'+file-templates|check)
