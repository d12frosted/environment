;;; +org-buffer-prop.el --- Utilities for working with buffer/tree properties -*- lexical-binding: t; -*-
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
(defun +org-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)") (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun +org-buffer-prop-set (name value)
  "Set a buffer property called NAME to VALUE."
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
(defun +org-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS."
  (split-string (+org-buffer-prop-get name) separators))

;;;###autoload
(defmacro def-org-buffer-prop (name val hook prop doc)
  "Define a buffer property with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer property named PROP.

DOC is used as a documentation string."
  `(def-org-buffer-prop-generic
     ,name
     ,val
     ,hook
     (+org-buffer-prop-get ,prop)
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: value"
              doc
              prop)))

;;;###autoload
(defmacro def-org-buffer-prop-list (name val sep hook prop doc)
  "Define a buffer property with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer property named PROP
and then split by SEP to become a list.

DOC is used as a documentation string."
  `(def-org-buffer-prop-generic
     ,name
     ,val
     ,hook
     (split-string-and-unquote (+org-buffer-prop-get ,prop) ,sep)
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: values

VALUES must be separated by '%s'."
              doc
              prop
              sep)))

;;;###autoload
(defmacro def-org-buffer-prop-brain-entry (name hook prop doc)
  "Define a buffer property with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer property named PROP.

DOC is used as a documentation string."
  `(def-org-buffer-prop-generic
     ,name
     nil
     ,hook
     (+brain-as-entry (+org-buffer-prop-get ,prop))
     ,(format "%s

Can be set in the org-mode buffer by adding following line in the
top of the file:

#+%s: ID"
              doc
              prop)))

;;;###autoload
(defmacro def-org-buffer-prop-generic (name val hook getter doc)
  "Define a buffer property with NAME and default value VAL.

Value is set on HOOK using a GETTER.

DOC is used as a documentation string."
  `(progn
     (defvar-local ,name ,val ,doc)
     (add-hook ,hook (lambda ()
                       (setq-local ,name ,getter)))))

(provide '+org-buffer-prop)
;;; +org-buffer-prop.el ends here
