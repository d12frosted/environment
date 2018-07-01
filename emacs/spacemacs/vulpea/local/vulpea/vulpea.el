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

(defun vulpea-brain--new-child (entry-or-id name)
  "Insert new entry with NAME as a child of ENTRY-OR-ID."
  (let ((entry (vulpea-brain--as-entry entry-or-id)))
    (if (org-brain-filep entry)
        ;; File entry
        (with-current-buffer (find-file-noselect (org-brain-entry-path entry))
          (goto-char (point-min))
          (if (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
              (progn
                (beginning-of-line)
                (open-line 1))
            (goto-char (point-max)))
          (insert (concat "* " name))
          (org-id-get-create))
      ;; Headline entry
      (org-with-point-at (org-brain-entry-marker entry)
        (if (org-goto-first-child)
            (open-line 1)
          (org-end-of-subtree t))
        (org-insert-heading)
        (org-do-demote)
        (insert name)
        (org-id-get-create)))))



;;;###autoload
(define-minor-mode vulpea-mode
  "Note taking utilities."
  :lighter " vulpea"
  (setq-local vulpea-properties-order (vulpea--get-buffer-properties-order))
  (setq-local vulpea-places-config (vulpea--get-buffer-places-config))

  (setq-local vulpea-cha-tea-groups-parent-id
              (vulpea--get-buffer-setting "TEA_GROUPS_PARENT"))
  (setq-local vulpea-cha--tea-groups-parent
              (vulpea-brain--as-entry vulpea-cha-tea-groups-parent-id))

  (setq-local vulpea-cha-fermentation-types-parent-id
              (vulpea--get-buffer-setting "FERMENTATION_TYPES_PARENT"))
  (setq-local vulpea-cha--fermentation-types-parent
              (vulpea-brain--as-entry vulpea-cha-fermentation-types-parent-id)))



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
  (seq-map
   (lambda (x)
     (let ((pairs (split-string x ":")))
       (cons (car pairs) (cdr pairs))))
   (vulpea--get-buffer-settings "PLACES_CONFIG")))



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
  (vulpea--get-buffer-settings "PROPERTIES_ORDER"))

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



(defvar-local vulpea-cha-tea-groups-parent-id ""
  "ID of Tea Groups parent entry.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+TEA_GROUPS_PARENT: ID")

(defvar-local vulpea-cha-fermentation-types-parent-id ""
  "ID of Fermentation types parent entry.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+FERMENTATION_TYPES_PARENT: ID")

(defvar-local vulpea-cha--fermentation-types-parent nil)
(defvar-local vulpea-cha--tea-groups-parent nil)

(defun vulpea-cha/new-tea-group ()
  "Create a new tea group."
  (interactive)
  (let* ((name (read-string "Tea group name: "))
         (id (vulpea-brain--new-child vulpea-cha--tea-groups-parent name)))
    (org-with-point-at (org-id-find id t)
      (vulpea--set-property-string "NAME_ORIGINAL")
      (vulpea--set-property-string "NAME_TRANSCRIPTION")
      (vulpea--set-property-string "NAME_MEANING")
      (vulpea--set-property-link "FERMENTATION"
                                 vulpea-cha--fermentation-types-parent)
      (save-buffer)
      (vulpea/pretty-entry-properties)
      (save-buffer))))



(defun vulpea--get-buffer-setting (name)
  "Get a setting called NAME from buffer as a string."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)") (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun vulpea--get-buffer-settings (name &optional separators)
  "Get a setting called NAME from buffer as a list using
SEPARATORS."
  (split-string (vulpea--get-buffer-setting name)))

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

(defun vulpea--set-property-string (name)
  (org-set-property name (read-string (concat name ": "))))

(defun vulpea--set-property-link (name parent)
  (org-set-property
   name
   (vulpea-brain--make-link
    (vulpea-brain--choose-entry-by-parent parent))))

(provide 'vulpea)

;;; vulpea.el ends here
