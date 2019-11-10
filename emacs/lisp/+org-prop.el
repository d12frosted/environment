;;; +org-prop.el --- Utilities for working with props -*- lexical-binding: t; -*-
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

(require '+org)
(require '+org-brain)
(require 'subr-x)

;;;###autoload
(defun +org-prompt-property (name &optional initial)
  "Prompt for a value and set is as property with NAME.

If INITIAL is non-nil, set it as initial value."
  (org-set-property
   name
   (read-string (concat (+org--pretty-property-prompt name) ": ") initial)))

;;;###autoload
(defun +org-prompt-number-property (name &optional initial)
  "Prompt for a number value and set is as property with NAME.

If INITIAL is non-nil, set it as initial value."
  (org-set-property
   name
   (number-to-string
    (read-number (concat (+org--pretty-property-prompt name) ": ") initial))))

;;;###autoload
(defun +org-prompt-brain-property (name parent &optional source type)
  "Prompt for a brain entry and set is as property with NAME.

Candidates are children of PARENT brain entry.

SOURCE and TYPE are passed to `+brain-make-link'."
  (org-set-property
   name
   (+brain-make-link
    (+brain-choose-entry-by-parent
     (format "%s: " (capitalize name))
     parent)
    source
    type)))

(defun +org--pretty-property-prompt (prop)
  "Create a pretty prompt from PROP."
  (capitalize (replace-regexp-in-string "_" " " prop)))

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
(defun +org-entry-set (prop val)
  "Set PROP of entry at point to VAL."
  (org-set-property prop val))

;;;###autoload
(defun +org-entry-set-number (prop num)
  "Set PROP of entry at point to NUM."
  (+org-entry-set prop (number-to-string num)))

;;;###autoload
(defun +org-entry-tag-p (tag)
  "Return non-nil when entry at point has TAG."
  (string-match-p (format ".*:%s:.*" tag) (or (org-entry-get nil "TAGS") "")))

;;;###autoload
(defun +org-entry-set-average-number (prop children-prop children-tag &optional child-hook)
  "Set the PROP of entry at point to average value of children.

CHILDREN-TAG is used to filter out children.

CHILDREN-PROP is used to get the number from matched children.

When CHILD-HOOK is non-nil it's being called with point at child."
  (when-let ((values (org-map-entries
                      (lambda ()
                        (when child-hook (funcall child-hook))
                        (+org-entry-get-number children-prop))
                      (concat "+" children-tag)
                      'tree)))
    (+org-entry-set-number prop
                           (if (null values)
                               0
                             (/ (apply #'+ values)
                                (float (length values)))))))

(provide '+org-prop)
;;; +org-prop.el ends here
