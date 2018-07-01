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

;;; Code:

(require 'org-brain)

(defun vulpea-brain/insert-link ()
  "Insert a link to brain entry."
  (interactive)
  (insert (vulpea-brain--make-link (vulpea-brain--choose-entry))))

(defun vulpea-brain--make-link (entry-or-id)
  "Make an org-mode link to ENTRY."
  (org-make-link-string
   (concat "brain:"
           (vulpea-brain--as-id entry-or-id))
   (org-brain-title (vulpea-brain--as-entry entry-or-id))))

(defun vulpea-brain--choose-entry ()
  "Choose a brain entry."
  (org-brain-choose-entry
   "Entry: "
   (append (org-brain-files t) (org-brain-headline-entries))
   nil
   t))

(defun vulpea-brain--choose-entry-by-parent (parent)
  "Choose a brain entry from children of PARENT."
  (org-brain-choose-entry
   "Entry: "
   (org-brain-children (vulpea-brain--as-entry parent))
   nil
   t))

(defun vulpea-brain--is-transitive-child-of (child parent)
  "Returns non-nil, when CHILD is a transitive child of PARENT."
  (let ((children (org-brain-children (vulpea-brain--as-entry parent))))
    (if (seq-contains children child #'vulpea-brain--entry-id-equal)
        t
      (seq-some (lambda (new-parent)
                  (vulpea-brain--is-transitive-child-of child new-parent))
                children))))

(defun vulpea-brain--transitive-children (parent)
  "Returns list of transitive children of PARENT."
  (seq-mapcat
   (lambda (entry)
     (seq-concatenate
      'list
      (list entry)
      (vulpea-brain--transitive-children entry)))
   (org-brain-children (vulpea-brain--as-entry parent))))

(defun vulpea-brain--as-entry (entry-or-id)
  "Returns brain entry."
  (if (stringp entry-or-id)
      (org-brain-entry-from-id entry-or-id)
    entry-or-id))

(defun vulpea-brain--as-id (entry-or-id)
  "Returns identifier of ENTRY-OR-ID."
  (if (stringp entry-or-id)
      entry-or-id
    (org-brain-entry-identifier entry-or-id)))

(defun vulpea-brain--entry-id-equal (a b)
  "Returns non-nil, when id of A equals id of B."
  (string-equal (vulpea-brain--as-id a)
                (vulpea-brain--as-id b)))



(define-minor-mode vulpea-mode
  "Note taking utilities."
  :lighter " vulpea"
  (setq-local vulpea-properties-order (vulpea--get-buffer-properties-order))
  (setq-local vulpea-properties-order (vulpea--get-buffer-places-config)))



(defvar-local vulpea-places-config '()
  "Association list of place level and it's entry ID.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+PROPERTIES_ORDER: LEVEL1:ID1 LEVEL2:ID2 LEVEL3:ID3 ...")

(defun vulpea/set-place-dwim ()
  (interactive)
  (let* ((level (completing-read "Level: " (seq-map #'car vulpea-places-config) nil t))
         (level-id (cdr (assoc-string level vulpea-places-config)))
         (level-entries (seq-map (lambda (x)
                                   (cons (car x)
                                         (org-brain-entry-from-id (cdr x))))
                                 vulpea-places-config))
         (entry (vulpea-brain--choose-entry-by-parent level-id))
         (entry-id (org-brain-entry-identifier entry)))

    ;; clear all levels
    (seq-do (lambda (level-cfg)
              (org-entry-delete nil (upcase (car level-cfg))))
            vulpea-places-config)

    (seq-do
     (lambda (x)
       (org-set-property (upcase (car x))
                         (vulpea-brain--make-link (cdr x))))
     ;; list of (level . specific-place-id) to set
     (seq-map
      (lambda (level-cfg)
        (cons (car level-cfg)
              (car (seq-filter
                    (lambda (e)
                      (vulpea-brain--is-transitive-child-of entry-id e))
                    (org-brain-children (cdr level-cfg))))))
      ;; list of levels to set
      (seq-filter
       (lambda (level-cfg)
         (and (not (string-equal level (car level-cfg)))
              (vulpea-brain--is-transitive-child-of entry-id (cdr level-cfg))))
       level-entries)))

    ;; set the level value
    (org-set-property (upcase level)
                      (vulpea-brain--make-link entry))))

(defun vulpea--get-buffer-places-config ()
  "Get the `vulpea-places-config' from current buffer."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+PLACES_CONFIG: \\(.*\\)" (point-max) t)
      (seq-map
       (lambda (x)
         (let ((pairs (split-string x ":")))
           (cons (car pairs) (cdr pairs))))
       (split-string (buffer-substring-no-properties
                      (match-beginning 1)
                      (match-end 1)))))))



(defvar-local vulpea-properties-order '()
  "List of properties used for ordering.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+PROPERTIES_ORDER: PROP1 PROP2 PROP3 ...")

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

(defun vulpea--get-buffer-properties-order ()
  "Get the `vulpea-properties-order' from current buffer."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+PROPERTIES_ORDER: \\(.*\\)" (point-max) t)
      (split-string (buffer-substring-no-properties
                     (match-beginning 1)
                     (match-end 1))))))

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
