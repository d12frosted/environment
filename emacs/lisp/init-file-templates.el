;;; lisp/init-file-templates.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 22 Oct 2019
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
  (expand-file-name "templates/" +path-emacs-dir)
  "The path to a directory of yasnippet folders to use for file templates.")

(defvar +file-templates-default-trigger "__"
  "The default yasnippet trigger key (a string) for file template
rules that don't have a :trigger property in
`+file-templates-alist'.")

(defvar +file-templates-alist
  `(;; elisp
    ("/.dir-locals.el$")
    (emacs-lisp-mode
     :trigger "__package")
    (snippet-mode))
  "An alist of file template rules. 

The CAR of each rule is either a major mode symbol or regexp
string. 

The CDR is a plist. See `+file-templates-set' for more
information.")

(use-package yasnippet
  :diminish
  :defer t
  :config
  (setq yas-prompt-functions (delq #'yas-dropdown-prompt yas-prompt-functions)
	yas-snippet-dirs '(+file-templates-dir))
  ;; Ensure file templates in `+file-templates-dir' are visible
    (yas-reload-all))

(defun +file-templates-check ()
  "Check if the current buffer is a candidate for file template
expansion. It must be non-read-only, empty, and there must be a
rule in `+file-templates-alist' that applies to it."
  (when (and (not buffer-read-only)
             (bobp) (eobp)
             (not (string-match-p "^ *\\*" (buffer-name))))
    (when-let* ((rule (cl-find-if #'+file-template-p +file-templates-alist)))
      (apply #'+file-templates--expand rule))))

(add-hook 'find-file-hook #'+file-templates-check)

(defun +file-templates--set (pred plist)
  (if (null (car-safe plist))
      (setq +file-templates-alist
            (delq (assoc pred +file-templates-alist)
                  +file-templates-alist))
    (push `(,pred ,@plist) +file-templates-alist)))

(defun +file-templates-set (pred &rest plist)
  "Register a file template.

PRED can either be a regexp string or a major mode symbol.

PLIST may contain these properties:

  :when FUNCTION
    Provides a secondary predicate. This function takes no
    arguments and is executed from within the target buffer. If
    it returns nil, this rule will be skipped over.

  :trigger STRING|FUNCTION
    If a string, this is the yasnippet trigger keyword used to
    trigger the target snippet.

    If a function, this function will be run in the context of
    the buffer to insert a file template into. It is given no
    arguments and must insert text into the current buffer
    manually.

    If omitted, `+file-templates-default-trigger' is used.

  :mode SYMBOL
    What mode to get the yasnippet snippet from. If omitted,
    either PRED (if it's a major-mode symbol) or the mode of the
    buffer is used.

  :project BOOL
    If non-nil, ignore this template if this buffer isn't in a
    project.

  :ignore BOOL
    If non-nil, don't expand any template for this file and don't
    test any other file template rule against this buffer.

\(fn PRED &key WHEN TRIGGER MODE PROJECT IGNORE)"
  (declare (indent defun))
  (+file-templates--set pred plist))

;;;###autoload
(cl-defun +file-templates--expand (pred &key project mode trigger ignore _when)
  "Auto insert a yasnippet snippet into current file and enter
insert mode (if evil is loaded and enabled)."
  (when (and pred (not ignore))
    (when (if project (+project-p) t)
      (unless mode
        (setq mode (if (symbolp pred) pred major-mode)))
      (unless mode
        (user-error "Couldn't determine mode for %s file template" pred))
      (unless trigger
        (setq trigger +file-templates-default-trigger))
      (if (functionp trigger)
          (funcall trigger)
        (require 'yasnippet)
        (unless yas-minor-mode
          (yas-minor-mode-on))
        (when (and yas-minor-mode
                   (when-let*
                       ((template (cl-find trigger (yas--all-templates (yas--get-snippet-tables mode))
                                           :key #'yas--template-key :test #'equal)))
                     (yas-expand-snippet (yas--template-content template)))
                   (and (featurep 'evil) evil-mode)
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))
          (evil-initialize-state 'insert))))))

;;;###autoload
(defun +file-template-p (rule)
  "Return t if RULE applies to the current buffer."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (cond ((and (stringp pred) buffer-file-name) (string-match-p pred buffer-file-name))
               ((symbolp pred) (eq major-mode pred)))
         (or (not (plist-member plist :when))
             (funcall (plist-get plist :when) buffer-file-name))
         rule)))

;;;###autoload
(defun +file-templates/debug ()
  "Tests the current buffer and outputs the file template rule
most appropriate for it. This is used for testing."
  (interactive)
  (message "Found %s"
	   (cl-find-if #'+file-template-p +file-templates-alist)))

(provide 'init-file-templates)
;;; init-file-templates.el ends here
