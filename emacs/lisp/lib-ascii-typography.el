;;; lib-ascii-typography.el --- Convert typographic characters to ASCII -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 28 Nov 2025
;;
;; URL: https://github.com/d12frosted/
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
;;; Code:

(require 'org)

(defun ascii-typography ()
  "Convert typographic characters to ASCII.
In Org mode, respects markup and skips protected regions."
  (interactive)
  (let ((replacements
         `((,(string #x201c) . "\"")   ; "
           (,(string #x201d) . "\"")   ; "
           (,(string #x201e) . "\"")   ; „
           (,(string #x00ab) . "\"")   ; «
           (,(string #x00bb) . "\"")   ; »
           (,(string #x2018) . "'")    ; '
           (,(string #x2019) . "'")    ; '
           (,(string #x201a) . "'")    ; ‚
           (,(string #x2039) . "'")    ; ‹
           (,(string #x203a) . "'")    ; ›
           (,(string #x2014) . "-")    ; — em dash
           (,(string #x2013) . "-")    ; – en dash
           (,(string #x2026) . "...")  ; …
           (,(string #x00a0) . " ")    ; non-breaking space
           (,(string #x2212) . "-")))) ; − minus sign
    (save-excursion
      (dolist (pair replacements)
        (goto-char (point-min))
        (while (search-forward (car pair) nil t)
          (save-match-data
            (when (ascii-typography--should-replace-p)
              (set-match-data (list (- (point) 1) (point)))
              (replace-match (cdr pair) t t))))))))

(defun ascii-typography--should-replace-p ()
  "Return non-nil if replacement at point is safe."
  (if (derived-mode-p 'org-mode)
      (ascii-typography--org-safe-p)
    t))

(defun ascii-typography--org-safe-p ()
  "Return non-nil if point is safe to modify in Org mode."
  (let ((context (org-element-context))
        (element (org-element-at-point)))
    (not (or
          (eq (org-element-type context) 'link)
          (memq (org-element-type context) '(verbatim code))
          (memq (org-element-type element)
                '(src-block example-block fixed-width comment-block
                  property-drawer node-property keyword))))))

(provide 'lib-ascii-typography)
;;; lib-ascii-typography.el ends here
