;;; org-generate.el --- generate org items

;; Copyright (c) 2015-2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 26 Jan 2017

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; Code belongs to lawlist, taken from http://stackoverflow.com/a/22419713/3086454

;;; Code:
;;

(defun org-generate (type org-file level-one
  &optional level-two full-level-two plain-list)
"Formating options for `org-capture-entry` are similar to `org-capture-templates`.
However, the first two elements (i.e., `:key` and `:description`) are NOT used.
Please see the doc-string of the variable `org-capture-templates` for more info.
  (1) `type`:  required -- 'entry | 'item
  (2) `org-file`:  required -- path to the org-mode file.
  (3) `level-one`:  required -- main heading.
  (4) `level-two`:  optional -- sub-heading headline (only).
  (5) `full-level-two`:  optional -- complete sub-heading.
  (6) `plain-list`:  optional -- a list.
EXAMPLES:
  `(org-generate 'entry org-file level-one level-two full-level-two)`
  `(org-generate 'item org-file level-one level-two nil plain-list)` "
  (require 'org-capture)
  (let (org-capture-entry)
    (cond
      ((eq type 'entry)
        (setq org-capture-entry
          `(entry
            (file+headline ,org-file ,level-one)
              ,full-level-two :empty-lines 1 :immediate-finish t))
        (org-generate-capture))
      ((eq type 'item)
        (setq org-capture-entry
          `(item
            (file+olp ,org-file ,level-one ,level-two)
              nil :empty-lines 1 :immediate-finish t))
        (mapcar (lambda (x)
          (progn
            (setcar (nthcdr 2 org-capture-entry) x)
            (org-generate-capture) ))
          plain-list)))))

(defun org-generate-capture ()
    (let* ((orig-buf (current-buffer))
     (annotation (if (and (boundp 'org-capture-link-is-already-stored)
        org-capture-link-is-already-stored)
         (plist-get org-store-link-plist :annotation)
       (ignore-errors (org-store-link nil))))
     (entry org-capture-entry)
     initial)
      (setq initial (or org-capture-initial
      (and (org-region-active-p)
           (buffer-substring (point) (mark)))))
      (when (stringp initial)
  (remove-text-properties 0 (length initial) '(read-only t) initial))
      (when (stringp annotation)
  (remove-text-properties 0 (length annotation)
        '(read-only t) annotation))
  (setq org-capture-plist (copy-sequence (nthcdr 3 entry)))
  (org-capture-put :target (nth 1 entry))
  (let ((txt (nth 2 entry)) (type (or (nth 0 entry) 'entry)))
    (when (or (not txt) (and (stringp txt) (not (string-match "\\S-" txt))))
      (cond
       ((eq type 'item) (setq txt "- %?"))
       ((eq type 'checkitem) (setq txt "- [ ] %?"))
       ((eq type 'table-line) (setq txt "| %? |"))
       ((member type '(nil entry)) (setq txt "* %?\n  %a"))))
    (org-capture-put :template txt :type type))
  (org-capture-get-template)
  (org-capture-put :original-buffer orig-buf
       :original-file (or (buffer-file-name orig-buf)
              (and (featurep 'dired)
             (car (rassq orig-buf
                   dired-buffers))))
       :original-file-nondirectory
       (and (buffer-file-name orig-buf)
            (file-name-nondirectory
             (buffer-file-name orig-buf)))
       :annotation annotation
       :initial initial
       :return-to-wconf (current-window-configuration)
       :default-time
       (or org-overriding-default-time
           (org-current-time)))
  (org-capture-set-target-location)
  (condition-case error
      (org-capture-put :template (org-capture-fill-template))
    ((error quit)
     (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
     (error "Capture abort: %s" error)))
  (setq org-capture-clock-keep (org-capture-get :clock-keep))
    (condition-case error
        (org-capture-place-template
         (equal (car (org-capture-get :target)) 'function))
      ((error quit)
       (if (and (buffer-base-buffer (current-buffer))
          (string-match "\\`CAPTURE-" (buffer-name)))
     (kill-buffer (current-buffer)))
       (set-window-configuration (org-capture-get :return-to-wconf))
       (error "Error.")))
    (if (and (derived-mode-p 'org-mode)
       (org-capture-get :clock-in))
        (condition-case nil
      (progn
        (if (org-clock-is-active)
      (org-capture-put :interrupted-clock
           (copy-marker org-clock-marker)))
        (org-clock-in)
        (org-set-local 'org-capture-clock-was-started t))
    (error
     "Could not start the clock in this capture buffer")))
    (if (org-capture-get :immediate-finish)
        (org-capture-finalize))))

(provide 'org-generate)

;;; org-generate.el ends here
