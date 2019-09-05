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

;;;###autoload
(defun +org-entry-get (prop)
  "Get PROP value of entry at point."
  (org-entry-get nil prop))

;;;###autoload
(defun +org-entry-get-number (prop &optional def)
  "Get number PROP value of entry at point.

Unless DEF is specified, nil is returned when PROP is not set.

Be careful though, as it uses `string-to-number' for conversion."
  (if-let ((str (+org-entry-get prop)))
      (string-to-number str)
    def))

;;;###autoload
(defun +org-entry-set-number (prop num)
  "Set PROP of entry to point to NUM."
  (org-set-property prop (number-to-string num)))

;;;###autoload
(defun +org-entry-tag-p (tag)
  "Return non-nil when entry at point has TAG."
  (string-match-p (format ".*:%s:.*" tag) (or (org-entry-get nil "TAGS") "")))
