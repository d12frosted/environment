;;; lib-litnotes.el --- Utilities for working with litnotes -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.io>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.2"))
;;
;; Created: 27 May 2021
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
;; This module provides various utilities for viewing and managing
;; litnotes.
;;
;;; Code:

(require 'vulpea)
(require 'lister)
(require 'lister-highlight)
(require 'all-the-icons)



(defconst litnotes-tag "litnotes"
  "Tag of all them litnotes.")



(defface litnotes-group-title-face
  '((t (:inherit org-roam-header-line)))
  "Face for displaying group title."
  :group 'litnotes)

(defface litnotes-group-counter-face
  '((t (:inherit font-lock-comment-face)))
  "Face for displaying group counter."
  :group 'litnotes)

(defface litnotes-entry-title-face
  '((t (:inherit org-document-title)))
  "Face for displaying entry title."
  :group 'litnotes)

(defface litnotes-entry-authors-face
  '((t (:inherit font-lock-comment-face)))
  "Face for displaying entry authors."
  :group 'litnotes)



(defvar litnotes-status-values '("ongoing" "new" "done" "dropped")
  "List with all valid values of status.")

(defun litnotes-status-compare (a b)
  "Compare status A with status B."
  (< (seq-position litnotes-status-values a)
     (seq-position litnotes-status-values b)))

(defun litnotes-status-display (status)
  "Display STATUS."
  (let ((icon (pcase status
                (`"ongoing" (all-the-icons-faicon "spinner"))
                (`"new" (all-the-icons-faicon "inbox"))
                (`"done" (all-the-icons-faicon "check"))
                (`"dropped" (all-the-icons-faicon "times")))))
    (if (featurep 'all-the-icons)
        (concat icon " " status)
      status)))

(defconst litnotes-status-tag-prefix "status/"
  "Prefix of the status tag.")

(defun litnotes-status-to-tag (status)
  "Return a tag representing STATUS."
  (concat litnotes-status-tag-prefix status))

(defun litnotes-status-from-tag (tag)
  "Return a status representing as TAG."
  (string-remove-prefix litnotes-status-tag-prefix tag))

(defun litnotes-status-tag-p (tag)
  "Return non-nil when TAG represents a status."
  (string-prefix-p litnotes-status-tag-prefix tag))

(defun litnotes-status-read (&optional old-status)
  "Read a status excluding OLD-STATUS."
  (completing-read
   "Status: "
   (-remove-item old-status litnotes-status-values)))



(defvar litnotes-content-types '("book"
                                 "article"
                                 "video"
                                 "course"
                                 "game")
  "List with all valid content types.")

(defun litnotes-content-compare (a b)
  "Compare content A with content B."
  (< (seq-position litnotes-content-types a)
     (seq-position litnotes-content-types b)))

(defun litnotes-content-display (content)
  "Display CONTENT."
  (let ((icon (pcase content
                (`"book" (all-the-icons-faicon "book"))
                (`"article" (all-the-icons-faicon "align-justify"))
                (`"video" (all-the-icons-material "videocam"))
                (`"game" (all-the-icons-faicon "gamepad")))))
    (if (and icon (featurep 'all-the-icons))
        (concat icon " ")
      "")))

(defconst litnotes-content-tag-prefix "content/"
  "Prefix of the content tag.")

(defun litnotes-content-to-tag (content)
  "Return a tag representing CONTENT."
  (concat litnotes-content-tag-prefix content))

(defun litnotes-content-from-tag (tag)
  "Return a content representing as TAG."
  (string-remove-prefix litnotes-content-tag-prefix tag))

(defun litnotes-content-tag-p (tag)
  "Return non-nil when TAG represents a content."
  (string-prefix-p litnotes-content-tag-prefix tag))



(cl-defstruct litnotes-entry
  note
  title
  meta
  status
  content
  authors)

(defun litnotes-entry (note)
  "Create a `litnotes-entry' from NOTE."
  (let ((meta (vulpea-meta note)))
    (make-litnotes-entry
     :note note
     :title (vulpea-note-title note)
     :meta meta
     :status (litnotes-status-from-tag
              (seq-find
               #'litnotes-status-tag-p
               (vulpea-note-tags note)))
     :content (string-remove-prefix
               "content/"
               (seq-find
                (lambda (x)
                  (string-prefix-p "content/" x))
                (vulpea-note-tags note)))
     :authors (vulpea-buffer-meta-get-list!
               meta "authors" 'note))))

(defun litnotes-entry-visit (entry &optional other-window)
  "Visit a litnote ENTRY possible in OTHER-WINDOW."
  (org-roam-node-visit
   (org-roam-node-from-id
    (vulpea-note-id (litnotes-entry-note entry)))
   other-window))



(defun litnotes-entries ()
  "Fetch a list of `litnotes-entry' entries."
  (seq-map
   #'litnotes-entry
   (seq-remove
    #'vulpea-note-primary-title
    (vulpea-db-query
     (lambda (x)
       (seq-contains-p (vulpea-note-tags x)
                       litnotes-tag))))))



(defvar-local litnotes-buffer-data nil
  "Associative list of all litnotes grouped by status.")

;;;###autoload
(define-derived-mode litnotes-mode
  lister-mode "litnotes"
  "Major mode for browsing litnotes."
  (lister-setup (current-buffer) #'litnotes-buffer-mapper))

(defconst litnotes-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit standard key bindings:
    (set-keymap-parent map lister-mode-map)
    (define-key map "\t"          #'litnotes-buffer-expand)
    (define-key map (kbd "<RET>") #'litnotes-buffer-visit)
    (define-key map (kbd "g")     #'litnotes-buffer-refresh)
    (define-key map (kbd "s")     #'litnotes-buffer-set-status)
    map)
  "Key map for `litnotes-mode'.")

;;;###autoload
(defun litnotes ()
  "Display a list of litnotes."
  (interactive)
  (let* ((name "*litnotes*")
         (_ (and (get-buffer name)
                 (kill-buffer name)))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (litnotes-mode)
      (setq litnotes-buffer-data (litnotes-buffer-data))
      (lister-highlight-mode 1)
      (lister-insert-sequence
       buffer (point) litnotes-status-values)
      (lister-goto buffer :first)
      (litnotes-buffer-expand-sublist buffer (point)))
    (switch-to-buffer buffer)))

(defun litnotes-buffer-data ()
  "Get data for litnotes buffer."
  (seq-sort-by
   #'car
   #'litnotes-status-compare
   (seq-group-by #'litnotes-entry-status (litnotes-entries))))

(defun litnotes-buffer-mapper (data)
  "DATA mapper for `litnotes-mode'."
  (if (stringp data)
      (concat
       (propertize
        (litnotes-status-display data)
        'face 'litnotes-group-title-face)
       " "
       (propertize
        (concat "("
                (number-to-string
                 (length (cdr (assoc data litnotes-buffer-data))))
                ")")
        'face 'litnotes-group-counter-face))
    (concat
     (litnotes-content-display
      (litnotes-entry-content data))
     (propertize
      (litnotes-entry-title data)
      'face 'litnotes-entry-title-face)
     (when (litnotes-entry-authors data)
       (concat
        " by "
        (string-join
         (seq-map
          (lambda (note)
            (propertize
             (vulpea-note-title note)
             'face 'litnotes-entry-authors-face))
          (litnotes-entry-authors data))
         ", "))))))

(defun litnotes-buffer-groups-refresh (buffer)
  "Refresh groups in litnotes BUFFER."
  (lister-walk-all
   buffer
   (lambda (data)
     (lister-replace buffer (point) data))
   #'stringp))

(defun litnotes-buffer-refresh ()
  "Refresh litnotes buffer."
  (interactive)
  (let ((pos (point)))
    (litnotes)
    (goto-char pos)))

(defun litnotes-buffer-expand (buffer pos)
  "Perform expansion on item at POS in litnotes BUFFER."
  (interactive (list (current-buffer) (point)))
  (let ((item (lister-get-data buffer pos)))
    (cond
     ((litnotes-entry-p item)
      (litnotes-entry-visit item 'other-window))

     ((ignore-errors (lister-sublist-below-p buffer pos))
      (lister-remove-sublist-below buffer pos))

     (t (litnotes-buffer-expand-sublist buffer pos)))))

(defun litnotes-buffer-expand-sublist (buffer pos)
  "Expand litnotes in the current list.

BUFFER must be a valid lister buffer populated with litnotes
items. POS can be an integer or the symbol `:point'."
  (interactive (list (current-buffer) (point)))
  (let* ((position
          (pcase pos
            ((and (pred integerp) pos) pos)
            (:point (with-current-buffer buffer (point)))
            (_ (error "Invalid value for POS: %s" pos))))
         (item (lister-get-data buffer position))
         (sublist (cdr (assoc item litnotes-buffer-data))))
    (if sublist
        (with-temp-message "Inserting expansion results..."
          (lister-insert-sublist-below buffer position sublist))
      (user-error "No expansion found"))))

(defun litnotes-buffer-visit (buffer pos)
  "Visit a litnote at POS from BUFFER."
  (interactive (list (current-buffer) (point)))
  (let* ((item (lister-get-data buffer pos)))
    (if (litnotes-entry-p item)
        (litnotes-entry-visit item)
      (user-error "Not a litnote"))))

(defun litnotes-buffer-set-status ()
  "Set status of a litnote at point."
  (interactive)
  (let* ((buffer (current-buffer))
         (pos (point))
         (item (lister-get-data buffer pos)))
    (if (litnotes-entry-p item)
        (let* ((old-status (litnotes-entry-status item))
               (status (litnotes-status-read old-status))
               (note (litnotes-entry-note item))
               (file (vulpea-note-path note)))
          (vulpea-utils-with-file file
            (litnotes-status-set status))
          (setf (litnotes-entry-status item) status)
          (setq litnotes-buffer-data
                (litnotes-buffer-data-change-group
                 item old-status status))
          (litnotes-buffer-change-group buffer pos item status)
          (litnotes-buffer-groups-refresh buffer))
      (user-error "Not a litnote"))))

(defun litnotes-buffer-change-group (buffer pos item new-group)
  "Move ITEM at POS to NEW-GROUP in litnotes BUFFER."
  ;; basically, remove whatever is in point
  (lister-remove buffer pos)

  ;; add to new group if it's expanded
  (lister-walk-all
   buffer
   (lambda (_)
     (let ((pos (point)))
       (when (ignore-errors
               (lister-sublist-below-p buffer pos))
         (let* ((next-item (lister-end-of-lines buffer pos))
                (next-level (lister-get-level-at buffer next-item)))
           (lister-insert
            buffer
            next-item
            item
            next-level)))))
   (lambda (data)
     (and (stringp data)
          (string-equal new-group data)))))

(defun litnotes-buffer-data-change-group (item old-group new-group)
  "Move ITEM from OLD-GROUP to NEW-GROUP in cached data."
  (seq-map
   (lambda (kvs)
     (cond
      ((string-equal old-group (car kvs))
       (cons (car kvs)
             (seq-remove
              (lambda (x)
                (string-equal
                 (vulpea-note-id (litnotes-entry-note x))
                 (vulpea-note-id (litnotes-entry-note item))))
              (cdr kvs))))
      ((string-equal new-group (car kvs))
       (cons (car kvs)
             (cons item (cdr kvs))))
      (t kvs)))
   litnotes-buffer-data))

;; TODO: add filtering
;; TODO: add other groupings



;;;###autoload
(defun litnotes-ensure-filetags (tags)
  "Ensure that TAGS contain the right set of tags."
  (when (seq-contains-p tags litnotes-tag)
    (unless (seq-find #'litnotes-status-tag-p tags)
      (setq tags (cons (litnotes-status-to-tag "new") tags)))
    (unless (seq-find #'litnotes-content-tag-p tags)
      (setq tags (cons
                  (litnotes-content-to-tag
                   (completing-read
                    "Content:"
                    litnotes-content-types))
                  tags))))
  tags)



;;;###autoload
(defun litnotes-status-set (&optional status)
  "Change STATUS tag of the current litnote."
  (when-let*
      ((file (buffer-file-name (buffer-base-buffer)))
       (tags (vulpea-buffer-prop-get-list "filetags"))
       (old-status (litnotes-status-from-tag
                    (seq-find #'litnotes-status-tag-p tags)))
       (status (or status (litnotes-status-read old-status)))
       (new-tags (litnotes-tags-set-status tags status)))
    (vulpea-buffer-prop-set-list "filetags" new-tags)
    (org-roam-db-update-file file)
    (save-buffer)))



(defun litnotes-tags-set-status (tags status)
  "Add STATUS to TAGS and return result.

STATUS is converted into proper tag, an any other status tag is
removed from TAGS."
  (cons
   (litnotes-status-to-tag status)
   (seq-remove
    #'litnotes-status-tag-p
    tags)))



(provide 'lib-litnotes)
;;; lib-litnotes.el ends here
