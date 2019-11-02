;;; +orgability.el --- orgability extensions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Nov 2019
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
(require '+org-prop)

;;;###autoload
(defun +orgability/list ()
  "Show orgability list."
  (interactive)
  (org-agenda nil "r"))

;;;###autoload
(defun +orgability/set-missing-fields ()
  "WIP migration."
  (interactive)
  (+orgability--maybe-set-field "TITLE")
  (+orgability--maybe-set-field "AUTHORS"))

(defun +orgability--maybe-set-field (prop)
  "WIP migration with PROP."
  (unless (org-entry-get nil prop)
    (+org-prompt-property prop (org-entry-get nil "ITEM"))))

(provide '+orgability)

;;; +orgability.el ends here
