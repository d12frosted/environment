;;; lang/org/autoload/property.el -*- lexical-binding: t; -*-
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
(defun +org-prompt-property (name &optional initial)
  "Prompt for a value and set is as property with NAME.

If INITIAL is non-nil, set it as initial value."
  (org-set-property
   name
   (read-string (concat (+org--pretty-property-prompt name) ": ") initial)))

;;;###autoload
(defun +org-prompt-property-brain (name parent)
  "Prompt for a brain entry and set is as property with NAME.

Candidates are children of PARENT brain entry."
  (org-set-property
   name
   (+brain-make-link
    (+brain-choose-entry-by-parent
     (format "%s: " (capitalize name))
     parent))))

(defun +org--pretty-property-prompt (prompt)
  (capitalize (replace-regexp-in-string "_" " " prompt)))
