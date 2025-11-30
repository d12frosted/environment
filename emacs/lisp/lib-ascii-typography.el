;;; lib-ascii-typography.el --- Convert typographic characters to ASCII -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.2
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
(require 'org-element)

(defvar ascii-typography-replacements
  `((,(string #x201c) . "\"")   ; left double quote
    (,(string #x201d) . "\"")   ; right double quote
    (,(string #x201e) . "\"")   ; double low-9 quote
    (,(string #x00ab) . "\"")   ; left guillemet
    (,(string #x00bb) . "\"")   ; right guillemet
    (,(string #x2018) . "'")    ; left single quote
    (,(string #x2019) . "'")    ; right single quote
    (,(string #x201a) . "'")    ; single low-9 quote
    (,(string #x2039) . "'")    ; single left guillemet
    (,(string #x203a) . "'")    ; single right guillemet
    (,(string #x2026) . "...")  ; ellipsis
    (,(string #x00a0) . " ")    ; non-breaking space
    (,(string #x2212) . "-"))   ; minus sign
  "Alist of typographic characters to ASCII replacements.")

(defvar ascii-typography-dashes
  (list (string #x2014)   ; em-dash
        (string #x2013))  ; en-dash
  "List of dash characters that need special handling.")

;;;###autoload
(defun ascii-typography ()
  "Convert typographic characters to ASCII.
In Org mode, respects markup and skips protected regions like
links (URL part), code blocks, verbatim, etc."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (ascii-typography--org)
    (ascii-typography--plain)))

(defun ascii-typography--plain ()
  "Convert typographic characters in plain text (no protection)."
  (let ((dash-re (ascii-typography--dash-regex)))
    ;; First pass: dashes between words -> spaced
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward dash-re nil t)
        (replace-match "\\1 - \\3" t)))
    ;; Second pass: remaining dashes and other characters
    (ascii-typography--replace-all
     (append (mapcar (lambda (d) (cons d "-")) ascii-typography-dashes)
             ascii-typography-replacements))))

(defun ascii-typography--org ()
  "Convert typographic characters in Org mode with protection."
  ;; Build protected regions once for efficiency
  (let ((protected (ascii-typography--org-protected-regions))
        (dash-re (ascii-typography--dash-regex)))
    ;; First pass: dashes between words
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward dash-re nil t)
        (let ((dash-start (match-beginning 2))
              (dash-end (match-end 2)))
          (unless (ascii-typography--in-protected-p dash-start dash-end protected)
            (replace-match "\\1 - \\3" t)))))
    ;; Second pass: simple replacements
    (dolist (pair (append
                   (mapcar (lambda (d) (cons d "-")) ascii-typography-dashes)
                   ascii-typography-replacements))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward (car pair) nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (unless (ascii-typography--in-protected-p start end protected)
              (replace-match (cdr pair) t t))))))))

(defun ascii-typography--dash-regex ()
  "Return regex matching dashes between non-space characters."
  (let ((em (string #x2014))
        (en (string #x2013)))
    (concat "\\([^ \t\n]\\)\\(" em "\\|" en "\\)\\([^ \t\n]\\)")))

(defun ascii-typography--replace-all (replacements)
  "Apply REPLACEMENTS alist throughout buffer."
  (save-excursion
    (dolist (pair replacements)
      (goto-char (point-min))
      (while (search-forward (car pair) nil t)
        (replace-match (cdr pair) t t)))))

(defun ascii-typography--org-protected-regions ()
  "Return sorted list of (START . END) protected regions in buffer.
Protected regions include:
- Link URLs (but not descriptions)
- Verbatim and code spans
- Source blocks, example blocks, etc.
- Property drawers, keywords"
  (let (regions)
    (org-element-map (org-element-parse-buffer)
        '(link verbatim code src-block example-block
          fixed-width comment-block property-drawer keyword)
      (lambda (el)
        (let ((type (org-element-type el)))
          (cond
           ;; For links, protect only the path part, not description
           ((eq type 'link)
            (let ((begin (org-element-property :begin el))
                  (contents-begin (org-element-property :contents-begin el)))
              (when (and begin contents-begin)
                ;; Protect from [[ up to description start
                (push (cons begin (1- contents-begin)) regions))
              ;; Also protect closing ]]
              (let ((contents-end (org-element-property :contents-end el))
                    (end (org-element-property :end el)))
                (when (and contents-end end)
                  (push (cons contents-end end) regions)))
              ;; For links without description, protect entire link
              (unless contents-begin
                (push (cons (org-element-property :begin el)
                            (org-element-property :end el))
                      regions))))
           ;; For other elements, protect entirely
           (t
            (push (cons (org-element-property :begin el)
                        (org-element-property :end el))
                  regions))))))
    ;; Sort by start position for binary search efficiency
    (sort regions (lambda (a b) (< (car a) (car b))))))

(defun ascii-typography--in-protected-p (start end regions)
  "Return non-nil if range START..END overlaps any protected REGIONS."
  (cl-some (lambda (region)
             (and (< start (cdr region))
                  (> end (car region))))
           regions))

(provide 'lib-ascii-typography)
;;; lib-ascii-typography.el ends here
