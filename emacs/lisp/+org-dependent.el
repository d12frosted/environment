;;; +org-dependent.el --- dealing with dependent tasks -*- lexical-binding: t; -*-
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
;; This package defines simple functions for manipulating dependencies between
;; entries using `org-edna' package.
;;
;; There are two dependencies:
;;
;; - A blocks B
;; - B is blocked by A
;;
;; In order to add this dependency, use `+org-dep/add-blocker' function. It asks
;; for an entry (A) that blocks entry (B) at point and updates properties to
;; reflect that dependency:
;;
;; - To the entry A it adds a property BLOCKS with value ids(B_ID), or if the
;;   property already exists, just adds B_ID to the ids.
;; - In a similar fashion, A_ID is added to the list of ids under BLOCKER
;;   property.
;; - Additionally, entry A also gets a TRIGGER property that will try to update
;;   the status of entry B once A is completed.
;;
;; Blocked entry is automatically set to WAITING status when blocker is added.
;;
;;; Code:

(require 'subr-x)
(require '+org)

;;;###autoload
(defun +org-dep/add-blocker ()
  "Add a blocker for entry at point.

1. User selects a blocker for entry at point.

2. Selected entry id is added to the list of blockers under
   BLOCKER property of entry at point.

3. Id of entry at point is added to the list under BLOCKS
   property of selected blocker.

4. A trigger is added to the blocker entry in order to
   automatically update entry at point once blocker is completed.

5. Entry at point todo status is changed to WAITING to reflect
   that it's blocked."
  (interactive)
  (let ((blocker-id (org-id-get-with-outline-path-completion))
        (current-id (org-id-get-create)))
    (+org-dep--add-id "BLOCKER" blocker-id)
    (+org-dep/update-status)
    (save-excursion
      (org-id-goto blocker-id)
      (org-entry-put nil "TRIGGER" "blocked-by-self update-status!")
      (+org-dep--add-id "BLOCKS" current-id))))

;;;###autoload
(defun +org-dep/remove-blocker ()
  "Remove a blocker from entry at point."
  (interactive)
  (when-let*
      ((blockers (+org-dep--get "BLOCKER"))
       (names (seq-map
               (lambda (id)
                 (cons
                  (save-excursion
                    (org-id-goto id)
                    (org-entry-get nil "ITEM"))
                  id))
               blockers))
       (select (ivy-completing-read
                "Select blocker to remove:"
                names
                nil
                t))
       (blocker-id (cdr (assoc select names)))
       (current-id (org-id-get-create)))
    (+org-dep--remove-id "BLOCKER" blocker-id)
    (save-excursion
      (org-id-goto blocker-id)
      (+org-dep--remove-id "BLOCKS" current-id))))

;;;###autoload
(defun +org-dep/update-status ()
  "Update status of entry at point based on dependencies.

If the entry at point contains at least one blocker that is not
completed yet, then the status is WAITING, otherwise - TODO."
  (let ((org-todo-log-states '(("WAITING" time)
                               ("TODO" time))))
    (if (seq-reduce (lambda (a b) (and a b))
                    (seq-map #'+org-dep--is-done-p (+org-dep--get "BLOCKER"))
                    t)
        (org-todo "TODO")
      (org-todo "WAITING"))))

(defun +org-dep--is-done-p (id)
  "Return t if entry with ID is done, nil otherwise."
  (save-excursion
    (org-id-goto id)
    (and (org-entry-is-done-p) t)))

(defun +org-dep--get (prop)
  "Get list of entries from IDS object of PROP."
  (let* ((raw (org-entry-get nil prop))
         (str (when raw (replace-regexp-in-string "ids(\\(.*\\))" "\\1" raw))))
    (when raw (split-string str " "))))

(defun +org-dep--set-ids (prop ids)
  "Set IDS under PROP of entry at point."
  (if (null ids)
      (org-delete-property prop)
    (org-set-property prop (format "ids(%s)" (mapconcat #'identity ids " ")))))

(defun +org-dep--add-id (prop id)
  "Add ID to the IDS object under PROP of entry at point."
  (let ((ids (+org-dep--get prop)))
    (+org-dep--set-ids prop (cl-pushnew id ids))))

(defun +org-dep--remove-id (prop id)
  "Remove ID from the IDS object under PROP of entry at point."
  (let ((ids (+org-dep--get prop)))
    (+org-dep--set-ids prop (delete id ids))))

;;;###autoload
(defun org-edna-finder/blocked-by-self ()
  "Finder for org-edna of entries blocked by self."
  (seq-map (lambda (id)
             (save-excursion
               (org-id-goto id)
               (point-marker)))
           (+org-dep--get "BLOCKS")))

(provide '+org-dependent)
;;; +org-dependent.el ends here
