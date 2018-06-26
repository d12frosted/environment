;;; vulpea.el --- Collection of note taking helpers -*- lexical-binding: t -*-

;; Author: Boris Buliga
;; Maintainer: Boris Buliga
;; Version: 0.1
;; Package-Requires: ((org-mode "9.1.0") (org-brain "0.5"))
;; Homepage: https://github.com/d12frosted/environment/tree/master/emacs/spacemacs/vulpea/local/vulpea
;; Keywords: personal


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(defun vulpea/insert-brain-link ()
  "Insert a link to brain entry."
  (interactive)
  (insert (vulpea--make-brain-link (vulpea--choose-brain-entry))))

(defun vulpea--make-brain-link (entry)
  "Make an org-mode link to ENTRY."
  (org-make-link-string
   (concat "brain:"
           (org-brain-entry-identifier entry))
   (org-brain-title entry)))

(defun vulpea--choose-brain-entry ()
  "Choose a brain entry."
  (org-brain-choose-entry
   "Entry: "
   (append (org-brain-files t) (org-brain-headline-entries))
   nil
   t))

(defun vulpea--choose-brain-entry-by-parent (parent-id)
  "Choose a brain entry from children of PARENT-ID."
  (org-brain-choose-entry
   "Entry: "
   (org-brain-children (org-brain-entry-from-id parent-id))
   nil
   t))



(defvar vulpea-places-config
  '(("country" . "EC4A5BD7-71C4-479A-8BBB-8F022E78F52D")))

(defun vulpea/set-place ()
  "Set place properties to org entry at point."
  (interactive)
  (seq-each
   (lambda (cfg)
     (unless (org-entry-get nil (car cfg))
       (org-set-property (upcase (car cfg)) (vulpea--make-brain-link (vulpea--choose-brain-entry-by-parent (cdr cfg))))))
   vulpea-places-config))



(defvar vulpea-properties-order
  '("ID" "CUSTOM_ID"
    "TEA_GROUP"
    "TAG"
    "NAME" "NAME_ORIGINAL" "NAME_TRANSCRIPTION" "NAME_MEANING"
    "YEAR_GATHERED" "YEAR_MANUFACTURED"
    "PRESSING"
    "COUNTRY" "PROVINCE" "PREFECTURE" "COUNTY" "TOWNSHIP" "VILLAGE" "MOUNTAIN" "LAKE" "PLACE"
    "RATE"
    "PRICE"
    "AVAILABLE" "TOTAL_IN" "TOTAL_OUT"))

(defun vulpea/sort-entry-properties ()
  "Sort properties in entry at point."
  (interactive)
  (let ((p0 (car (org-get-property-block)))
        (p1 (- (cdr (org-get-property-block)) 1))
        (props (org-entry-properties))
        (maxv (seq-length vulpea-properties-order))
        (pregx "^:\\([a-zA-Z_\\-]+\\):.*$"))
    (save-excursion
      (save-restriction
        (narrow-to-region p0 p1)
        (goto-char (point-min))
        (let ;; To make `end-of-line' and etc. to ignore fields.
            ((inhibit-field-text-motion t))
          (sort-subr
           nil 'forward-line 'end-of-line nil nil
           (lambda (l1 l2)
             (< (or
                 (seq-position vulpea-properties-order
                               (vulpea--match-regexp pregx l1))
                 maxv)
                (or
                 (seq-position vulpea-properties-order
                               (vulpea--match-regexp pregx l2))
                 maxv)))))))))

(defun vulpea/format-entry-properties ()
  "Format properties in entry at point."
  (interactive)
  (let ((p0 (car (org-get-property-block)))
        (p1 (cdr (org-get-property-block))))
    (save-excursion
      (goto-char p0)
      (while (< (point) p1)
        (org-indent-line)
        (forward-line 1)
        (beginning-of-line)))))

(defun vulpea/pretty-entry-properties ()
  "Prettify properties of entry at point.

- `vulpea/format-entry-properties'
- `vulpea/sort-entry-properties'"
  (interactive)
  (vulpea/format-entry-properties)
  (vulpea/sort-entry-properties))

(defun vulpea/pretty-buffer-properties ()
  "Prettify properties of all entries in buffer.

- `vulpea/format-entry-properties'
- `vulpea/sort-entry-properties'"
  (interactive)
  (vulpea--map-outline
   (lambda ()
     (vulpea/format-entry-properties)
     (vulpea/sort-entry-properties))))



(defun vulpea--map-outline (f)
  "Call function F on every outline in buffer."
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward-regexp outline-regexp)
      (funcall f))))

(defun vulpea--match-regexp (regexp val)
  "Get the 1 match from the VAL."
  (let ((s (if (stringp val)
               val
             (buffer-substring (car val) (cdr val)))))
    (string-match regexp s)
    (match-string 1 s)))

(provide 'vulpea)

;;; vulpea.el ends here
