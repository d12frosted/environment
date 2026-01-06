;;; lib-org-margin.el --- Heading level indicators -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 25 Nov 2025
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
;; Replace org heading stars with level indicators. Uses font-lock
;; with display property for stable rendering (same technique as
;; org-superstar).
;;
;;; Code:

(require 'org)

(defgroup org-margin nil
  "Display heading levels in place of stars."
  :group 'org)

(defcustom org-margin-format "#%d" ;; "§%d"
  "Format string for heading level indicator.
%d is replaced with the heading level number."
  :type 'string
  :group 'org-margin)

(defface org-margin-indicator
  '((t :height 0.7 :weight light :inherit org-drawer))
  "Face for heading level indicators."
  :group 'org-margin)

(defun org-margin--make-indicator (level)
  "Create indicator string for heading LEVEL."
  (propertize (format org-margin-format level)
              'face 'org-margin-indicator))

(defun org-margin--fontify (limit)
  "Fontify heading stars up to LIMIT."
  (while (re-search-forward "^\\(\\*+\\) " limit t)
    (let* ((stars-beg (match-beginning 1))
           (stars-end (match-end 1))
           (level (- stars-end stars-beg))
           (indicator (org-margin--make-indicator level)))
      (put-text-property stars-beg stars-end 'display indicator)))
  nil)

(defun org-margin--clear-stale (beg end _len)
  "Clear stale display properties in region BEG to END after buffer change."
  (save-excursion
    (save-match-data
      (goto-char beg)
      (beginning-of-line)
      (let ((line-beg (point)))
        (goto-char end)
        (end-of-line)
        (remove-text-properties line-beg (point) '(display nil))))))

(defvar org-margin--font-lock-keywords
  '((org-margin--fontify))
  "Font-lock keywords for `org-margin-mode'.")

;;;###autoload
(define-minor-mode org-margin-mode
  "Replace heading stars with level indicators."
  :lighter ""
  :group 'org-margin
  (if org-margin-mode
      (progn
        (font-lock-add-keywords nil org-margin--font-lock-keywords 'append)
        (add-hook 'after-change-functions #'org-margin--clear-stale nil t)
        (font-lock-flush))
    (font-lock-remove-keywords nil org-margin--font-lock-keywords)
    (remove-hook 'after-change-functions #'org-margin--clear-stale t)
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(display nil)))
    (font-lock-flush)))

(provide 'lib-org-margin)
;;; lib-org-margin.el ends here
