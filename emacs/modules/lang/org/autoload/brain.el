;;; lang/org/autoload/brain.el -*- lexical-binding: t; -*-
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

(require 'org-brain)

;;;###autoload
(defun +brain/insert-link ()
  "Insert a link to brain entry."
  (interactive)
  (insert (+brain-make-link (+brain-choose-entry))))

;;;###autoload
(defun +brain-title (entry-or-id)
  "Get title of ENTRY-OR-ID."
  (save-match-data
    (org-brain-title (+brain-as-entry entry-or-id))))

;;;###autoload
(defun +brain-make-link (entry-or-id)
  "Make an org-mode link to ENTRY-OR-ID."
  (org-make-link-string
   (concat "brain:"
           (+brain-as-id entry-or-id))
   (+brain-title entry-or-id)))

;;;###autoload
(defun +brain-choose-entry ()
  "Choose a brain entry."
  (org-brain-choose-entry
   "Entry: "
   (append (org-brain-files t) (org-brain-headline-entries))
   nil
   t))

;;;###autoload
(defun +brain-choose-entry-by-parent (prompt parent)
  "Choose a brain entry from children of PARENT."
  (let ((org-brain-file-entries-use-title nil))
    (org-brain-choose-entry
     prompt
     (org-brain-children (+brain-as-entry parent))
     nil
     t)))

;;;###autoload
(defun +brain-is-child-of (child parent)
  "Returns non-nil, when CHILD is a child of PARENT."
  (let ((children (org-brain-children (brain-as-entry parent))))
    (seq-contains children child #'+brain-entry-id-equal)))

;;;###autoload
(defun +brain-is-transitive-child-of (child parent)
  "Returns non-nil, when CHILD is a transitive child of PARENT."
  (let ((children (org-brain-children (+brain-as-entry parent))))
    (if (seq-contains children child #'+brain-entry-id-equal)
        t
      (seq-some (lambda (new-parent)
                  (+brain-is-transitive-child-of child new-parent))
                children))))

;;;###autoload
(defun +brain-transitive-children (parent)
  "Returns list of transitive children of PARENT."
  (seq-mapcat
   (lambda (entry)
     (seq-concatenate
      'list
      (list entry)
      (+brain-transitive-children entry)))
   (org-brain-children (+brain-as-entry parent))))

;;;###autoload
(defun +brain-as-entry (entry-or-id)
  "Returns brain entry."
  (if (stringp entry-or-id)
      (org-brain-entry-from-id entry-or-id)
    entry-or-id))

;;;###autoload
(defun +brain-as-id (entry-or-id)
  "Returns identifier of ENTRY-OR-ID."
  (if (stringp entry-or-id)
      entry-or-id
    (org-brain-entry-identifier entry-or-id)))

;;;###autoload
(defun +brain-entry-id-equal (a b)
  "Returns non-nil, when id of A equals id of B."
  (string-equal (+brain-as-id a)
                (+brain-as-id b)))

;;;###autoload
(defun +brain-new-child (entry-or-id name)
  "Insert new entry with NAME as a child of ENTRY-OR-ID."
  (let ((entry (+brain-as-entry entry-or-id)))
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
(defun +brain-get-property (entry-or-id name)
  "Returns property with NAME of ENTRY-OR-ID."
  (+brain-map-entry
   entry-or-id
   (lambda (entry)
     (org-entry-get nil name))
   nil))

;;;###autoload
(defun +brain-map-entry (entry-or-id entry-fn file-fn)
  "Execute ENTRY-FN or FILE-FN on ENTRY-OR-ID.

If ENTRY-OR-ID points to file entry, then FILE-FN is executed.
Otherwise ENTRY-FN is executed.

Entry is passed to both functions as argument.

For the duration of every function, point is set on entry."
  (let ((entry (+brain-as-entry entry-or-id)))
    (if (org-brain-filep entry)
        (with-current-buffer (find-file-noselect (org-brain-entry-path entry))
          (goto-char (point-min))
          (unless (null file-fn)
            (funcall file-fn entry)))
      (org-with-point-at (org-brain-entry-marker entry)
        (unless (null entry-fn)
          (funcall entry-fn entry))))))
