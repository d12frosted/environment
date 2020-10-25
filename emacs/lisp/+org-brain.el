;;; +org-brain.el --- brain extension -*- lexical-binding: t; -*-
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

(autoload 'org-brain--internal-add-friendship "org-brain")
(autoload 'org-brain-add-friendship "org-brain")
(autoload 'org-brain-add-relationship "org-brain")
(autoload 'org-brain-children "org-brain")
(autoload 'org-brain-choose-entry "org-brain")
(autoload 'org-brain-entry-from-id "org-brain")
(autoload 'org-brain-entry-identifier "org-brain")
(autoload 'org-brain-entry-marker "org-brain")
(autoload 'org-brain-entry-path "org-brain")
(autoload 'org-brain-filep "org-brain")
(autoload 'org-brain-files "org-brain")
(autoload 'org-brain-headline-entries "org-brain")
(autoload 'org-brain-title "org-brain")

;;;###autoload
(defun +brain/insert-link ()
  "Insert a link to brain entry."
  (interactive)
  (insert (+brain-make-link (+brain-choose-entry))))

;;;###autoload
(defun +brain/insert-parent-link ()
  "Insert a link to brain entry and mark it as parent."
  (interactive)
  (insert (+brain-make-link
           (+brain-choose-entry)
           (+brain-as-entry (org-id-get-create))
           'parent)))

;;;###autoload
(defun +brain/insert-child-link ()
  "Insert a link to brain entry and mark it as child."
  (interactive)
  (insert (+brain-make-link
           (+brain-choose-entry)
           (+brain-as-entry (org-id-get-create))
           'child)))

;;;###autoload
(defun +brain/insert-friend-link ()
  "Insert a link to brain entry and mark it as friend."
  (interactive)
  (insert (+brain-make-link
           (+brain-choose-entry)
           (+brain-as-entry (org-id-get-create))
           'friend)))

;;;###autoload
(defun +brain-title (entry-or-id)
  "Get title of ENTRY-OR-ID."
  (save-match-data
    (org-brain-title (+brain-as-entry entry-or-id))))

;;;###autoload
(defun +brain-make-link (target-eoi &optional source-eoi type)
  "Make an `org-mode' link to TARGET-EOI from SOURCE-EOI.

TYPE is one of nil, parent, child, friend."
  (when (and type (symbolp type))
    (setq type (symbol-name type)))
  (pcase type
    ("parent"
     (org-brain-add-relationship (+brain-as-entry target-eoi)
                                 (+brain-as-entry source-eoi)))
    ("child"
     (org-brain-add-relationship (+brain-as-entry source-eoi)
                                 (+brain-as-entry target-eoi)))
    ("friend"
     (org-brain--internal-add-friendship (+brain-as-entry target-eoi)
                                         (+brain-as-entry source-eoi))
     (org-brain-add-friendship source-eoi (list target-eoi))))
  (org-link-make-string
   (concat "brain"
           (when type
             (concat "-" type))
           ":"
           (+brain-as-id target-eoi))
   (+brain-title target-eoi)))

;;;###autoload
(defun +brain-choose-entry ()
  "Choose a brain entry."
  (org-brain-choose-entry
   "Entry: "
   (append (org-brain-files t) (org-brain-headline-entries))
   nil
   t))

(defun +brain-choose-entry-by-parent (prompt parent)
  "PROMPT a brain entry from children of PARENT."
  (org-brain-choose-entry
   prompt
   (seq-filter (lambda (x) (not (null x)))
               (org-brain-children (+brain-as-entry parent)))
   nil
   t))

(defun +brain-choose-local-entry-by-parent (prompt parent)
  "PROMPT a brain entry from children of PARENT."
  (org-brain-choose-entry
   prompt
   (seq-filter (lambda (x) (not (null x)))
               (org-brain-local-children (+brain-as-entry parent)))
   nil
   t))

;;;###autoload
(defun +brain-is-child-of (child parent)
  "Return non-nil, when CHILD is a child of PARENT."
  (let ((children (org-brain-children (+brain-as-entry parent))))
    (seq-contains-p children child #'+brain-entry-id-equal)))

(defun +brain-is-transitive-child-of (child parent)
  "Return non-nil, when CHILD is a transitive child of PARENT."
  (let ((children (org-brain-children (+brain-as-entry parent))))
    (if (seq-contains-p children child #'+brain-entry-id-equal)
        t
      (seq-some (lambda (new-parent)
                  (+brain-is-transitive-child-of child new-parent))
                children))))

(defun +brain-is-local-transitive-child-of (child parent)
  "Return non-nil, when CHILD is a transitive child of PARENT."
  (let* ((children (org-brain-local-children (+brain-as-entry parent))))
    (if (seq-contains-p children child #'+brain-entry-id-equal)
        t
      (seq-some (lambda (new-parent)
                  (+brain-is-local-transitive-child-of child new-parent))
                children))))

;;;###autoload
(defun +brain-transitive-children (parent)
  "Return list of transitive children of PARENT."
  (seq-mapcat
   (lambda (entry)
     (seq-concatenate
      'list
      (list entry)
      (+brain-transitive-children entry)))
   (org-brain-children (+brain-as-entry parent))))

;;;###autoload
(defun +brain-as-entry (entry-or-id)
  "Return brain entry from ENTRY-OR-ID."
  (if (stringp entry-or-id)
      (org-brain-entry-from-id entry-or-id)
    entry-or-id))

;;;###autoload
(defun +brain-as-id (entry-or-id)
  "Return identifier of ENTRY-OR-ID."
  (if (stringp entry-or-id)
      entry-or-id
    (org-brain-entry-identifier entry-or-id)))

;;;###autoload
(defun +brain-entry-id-equal (a b)
  "Return non-nil, when id of A equals id of B."
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
  "Return property with NAME of ENTRY-OR-ID."
  (+brain-map-entry
   entry-or-id
   (lambda (_)
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


(provide '+org-brain)
;;; +org-brain.el ends here
