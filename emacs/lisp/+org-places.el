;;; +org-places.el --- utilities for places in the world -*- lexical-binding: t; -*-
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
(require '+org-brain)
(require '+org-link)
(require '+org-buffer-prop)

(defvar-local places-config '()
  "Association list of place level and it's entry ID.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+PLACES_CONFIG: LEVEL1:ID1:TYPE LEVEL2:ID2:TYPE LEVEL3:ID3:TYPE ...")

;;;###autoload
(define-minor-mode places-mode
  "Minor mode for buffers with places."
  :lighter " 🌏"
  (setq-local places-config (places-buffer-config)))

(defun places/set-dwim ()
  "Set the location properties for headline at point."
  (interactive)
  (let* ((level (completing-read "Level: " (seq-map #'car places-config) nil t))
         (level-config (assoc-string level places-config))
         (level-id (nth 1 level-config))
         (link-type (nth 2 level-config))
         (level-entries (seq-map (lambda (x)
                                   (cons (car x)
                                         (org-brain-entry-from-id (nth 1 x))))
                                 places-config))
         (entry (+brain-choose-entry-by-parent "Level: " level-id))
         (entry-id (org-brain-entry-identifier entry)))

    ;; clear all levels
    (seq-do (lambda (level-cfg)
              (org-entry-delete nil (upcase (car level-cfg))))
            places-config)

    (seq-do
     (lambda (x)
       (org-set-property (upcase (car x))
                         (+brain-make-link (cdr x))))

     ;; list of (level . specific-place-id) to set
     (seq-map
      (lambda (level-cfg)
        (cons (car level-cfg)
              (car (seq-filter
                    (lambda (e)
                      (+brain-is-local-transitive-child-of entry-id e))
                    (org-brain-children (cdr level-cfg))))))
      ;; list of levels to set
      (seq-filter
       (lambda (level-cfg)
         (and (not (string-equal level (car level-cfg)))
              (+brain-is-local-transitive-child-of entry-id (cdr level-cfg))))
       level-entries)))

    ;; set the level value
    (org-set-property (upcase level)
                      (+brain-make-link entry (org-id-get-create) link-type))))

(defun places-get-location-id ()
  "Get LOCATION id of entry at point."
  (seq-reduce
   (lambda (res level-cfg)
     (let ((val (org-entry-get nil (car level-cfg))))
       (if (or (null val) (string-empty-p val))
           res
         (+org-extract-id-from-link val))))
   places-config
   nil))

(defun places-get-location ()
  "Get LOCATION name of entry at point."
  (org-brain-title (+brain-as-entry (places-get-location-id))))

(defun places-buffer-config ()
  "Get the `places-config' from the current buffer."
  (seq-map
   (lambda (x) (split-string x ":"))
   (+org-buffer-prop-get-list "PLACES_CONFIG")))

(provide '+org-places)
;;; +org-places.el ends here
