;;; lang/org/autoload/settings.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Dec 2018
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

;;;###autoload
(defun +org-get-buffer-settings (name &optional separators)
  "Get a setting called NAME from buffer as a list using
SEPARATORS."
  (split-string (+org-get-buffer-setting name)))

;;;###autoload
(defmacro def-org-buffer-setting (name val hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP."
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
(defmacro def-org-buffer-brain-entry-setting (name hook prop doc)
  "Define a buffer setting with NAME and default value VAL.

Value is loaded on HOOK from the Org buffer setting named PROP."
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

Value is set on HOOK using getter"
  `(progn
     (defvar-local ,name ,val ,doc)
     (add-hook ,hook (lambda ()
                       (setq-local ,name ,getter)))))
