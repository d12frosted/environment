;;; +org-settings.el --- Utilities for working with buffer/tree settings -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Nov 2019
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

;;;###autoload
(defun +org-get-buffer-setting (name)
  "Get a setting called NAME from buffer as a string."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)") (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun +org-buffer-setting-set (name value)
  "Set a buffer setting called NAME to VALUE."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (if (re-search-forward (concat "^#\\+" name ": \\(.*\\)") (point-max) t)
        (replace-match (concat "#+" name ": " value))
      ;; find the first line that doesn't begin with ':' or '#'
      (let ((found))
        (while (not (or found (eobp)))
          (beginning-of-line)
          (if (or (looking-at "^#")
                  (looking-at "^:"))
              (line-move 1 t)
            (setq found t)
            (insert "#+" name ": " value "\n")))))))

;;;###autoload
(defun +org-get-buffer-settings (name &optional separators)
  "Get a setting NAME from buffer as a list using SEPARATORS."
  (split-string (+org-get-buffer-setting name) separators))

;;;###autoload
(defmacro def-org-buffer-setting (name val hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP.

DOC is used as a documentation string."
  `(def-org-buffer-setting-generic
     ,name
     ,val
     ,hook
     (+org-get-buffer-setting ,prop)
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: value"
              doc
              prop)))

;;;###autoload
(defmacro def-org-buffer-setting-list (name val sep hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP
and then split by SEP to become a list.

DOC is used as a documentation string."
  `(def-org-buffer-setting-generic
     ,name
     ,val
     ,hook
     (split-string-and-unquote (+org-get-buffer-setting ,prop) ,sep)
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: values

VALUES must be separated by '%s'."
              doc
              prop
              sep)))

;;;###autoload
(defmacro def-org-buffer-brain-entry-setting (name hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP.

DOC is used as a documentation string."
  `(def-org-buffer-setting-generic
     ,name
     nil
     ,hook
     (+brain-as-entry (+org-get-buffer-setting ,prop))
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: ID"
              doc
              prop)))

;;;###autoload
(defmacro def-org-buffer-setting-generic (name val hook getter doc)
  "Define a buffer setting with NAME and default value VAL.

Value is set on HOOK using a GETTER.

DOC is used as a documentation string."
  `(progn
     (defvar-local ,name ,val ,doc)
     (add-hook ,hook (lambda ()
                       (setq-local ,name ,getter)))))

(provide '+org-settings)
;;; +org-settings.el ends here
