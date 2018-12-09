;;; lang/org/autoload/cha.el -*- lexical-binding: t; -*-
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

;;;###autoload
(define-minor-mode cha-mode
  "Minor mode for all the tea utilities."
  :lighter " 茶"
  (places-mode)
  (pretty-props-mode)

  (setq-local cha-tea-groups-parent-id
              (+org-get-buffer-setting "TEA_GROUPS_PARENT"))
  ;; TODO use watchers
  (setq-local cha--tea-groups-parent
              (+brain-as-entry cha-tea-groups-parent-id))

  (setq-local cha-fermentation-types-parent-id
              (+org-get-buffer-setting "FERMENTATION_TYPES_PARENT"))
  (setq-local cha--fermentation-types-parent
              (+brain-as-entry cha-fermentation-types-parent-id))

  (setq-local cha-pressing-types-parent-id
              (+org-get-buffer-setting "PRESSING_TYPES_PARENT"))
  (setq-local cha--pressing-types-parent
              (+brain-as-entry cha-pressing-types-parent-id))

  (setq-local cha-tea-parent-id
              (+org-get-buffer-setting "TEA_PARENT"))
  (setq-local cha--tea-parent
              (+brain-as-entry cha-tea-parent-id))

  (setq-local cha-tea-title-format
              (+org-get-buffer-setting "TEA_TITLE_FORMAT"))

  (setq-local cha-default-currency
              (+org-get-buffer-setting "DEFAULT_CURRENCY")))

;;;###autoload
(defun cha-mode-maybe-enable ()
  "Conditionally enable `cha-mode' in the `org-mode' buffer.

Enables the `cha-mode' iff the buffer has 'cha-mode:t' option set
in the options section.

  #+OPTIONS: cha-mode:t"
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*cha-mode:t" (point-max) t)
        (cha-mode)))))

;;
;; Pretty

(defun cha/pretty-buffer ()
  "Prettify all tea entries in the buffer."
  (interactive)
  (let ((loc-groups (org-id-find cha-tea-groups-parent-id))
        (loc-tea (org-id-find cha-tea-parent-id)))

    (+org-with-file
     (car loc-groups)
     (org-with-point-at (cdr loc-groups)
       (org-map-entries #'pretty-props/entry nil 'tree)))

    (+org-with-file
     (car loc-tea)
     (org-with-point-at (cdr loc-tea)
       (org-map-entries (lambda ()
                          (if (string-equal (org-id-get)
                                            cha-tea-parent-id)
                              (pretty-props/entry)
                            (cha/pretty-tea)))
                        nil 'tree)))))

(defun cha/pretty-tea ()
  "Prettify tea entry at point."
  (interactive)
  (org-set-property
   "AVAILABLE"
   (number-to-string
    (- (string-to-number (or (org-entry-get nil "TOTAL_IN") ""))
       (string-to-number (or (org-entry-get nil "TOTAL_OUT") "")))))
  (org-edit-headline
   (cha-format-tea-title cha-tea-title-format))
  (pretty-props/entry))

;;
;; Title

(defvar-local cha-tea-title-format nil
  "Format of the Tea entry title.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+TEA_TITLE_FORMAT: {LOCATION} {NAME} | {YEAR} | {TAG}")

(defun cha-format-tea-title (format)
  (let ((properties (org-entry-properties))
        (val-regexp "{\\([a-zA-Z0-9\\-_]+\\)")
        (prop "")
        (val "")
        (result format))
    (map-put properties "LOCATION" (places-get-location))
    (map-put properties "YEAR" (cha-get-year))
    (while (string-match val-regexp result)
      (setq prop (match-string 1 result))
      (setq val (cdr (assoc-string prop properties)))
      (setq result (replace-regexp-in-string
                    (concat "{" prop "}")
                    val
                    result
                    t)))
    result))

;;
;; Year

(defun cha-get-year ()
  "Get YEAR of entry at point."
  (let ((year-gathered (org-entry-get nil "YEAR_GATHERED"))
        (year-manufactured (org-entry-get nil "YEAR_MANUFACTURED")))
    (cond
     ((string-equal year-gathered year-manufactured) year-gathered)
     ((null year-gathered) year-manufactured)
     ((null year-manufactured) year-gathered)
     (t (concat year-gathered " - " year-manufactured)))))

;;
;; Currency

(defvar-local cha-default-currency ""
  "Default currency.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+DEFAULT_CURRENCY: currency")

;;
;; Tea groups

(defvar cha-tea-groups-parent-id ""
  "ID of Tea Groups parent entry.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+TEA_GROUPS_PARENT: ID")

(defvar-local cha--tea-groups-parent nil)

(defun cha--read-tea-group ()
  "Read Tea Group."
  (+brain-choose-entry-by-parent
   "Tea group: "
   cha--tea-groups-parent))

(defun cha/new-tea-group ()
  "Create a new tea group entry."
  (interactive)
  (let* ((name (read-string "Tea group name: "))
         (id (+brain-new-child cha--tea-groups-parent name)))
    (org-with-point-at (org-id-find id t)
      (+org-prompt-property "NAME_ORIGINAL")
      (+org-prompt-property "NAME_TRANSCRIPTION")
      (+org-prompt-property "NAME_MEANING")
      (+org-prompt-property-brain "FERMENTATION"
                                  cha--fermentation-types-parent)
      (pretty-props/entry)
      (save-buffer))))

;;
;; Fermentation

(defvar cha-fermentation-types-parent-id ""
  "ID of Fermentation types parent entry.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+FERMENTATION_TYPES_PARENT: ID")

(defvar-local cha--fermentation-types-parent nil)

;;
;; Pressing

(defvar cha-pressing-types-parent-id ""
  "ID of Pressing types parent entry.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+PRESSING_TYPES_PARENT: ID")

(defvar-local cha--pressing-types-parent nil)

(defun cha--read-pressing ()
  "Read Tea Group."
  (+brain-choose-entry-by-parent
   "Pressing: "
   cha--pressing-types-parent))

(defun cha/set-pressing (&optional pressing)
  "Set PRESSING of tea entry at point"
  (interactive)
  (unless pressing
    (setq pressing (cha--read-pressing)))
  (org-set-property "PRESSING"
                    (+brain-make-link pressing)))

;;
;; Tea

(defvar cha-tea-parent-id ""
  "ID of Tea parent entry.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+TEA_PARENT: ID")

(defvar-local cha--tea-parent nil)

(defun cha/new-tea ()
  "Create a new tea entry."
  (interactive)
  (let* ((tea-group (cha--read-tea-group))
         (pressing (cha--read-pressing))
         (name (cadr tea-group))
         (name-original
          (+brain-get-property tea-group "NAME_ORIGINAL"))
         (name-transcription
          (+brain-get-property tea-group "NAME_TRANSCRIPTION"))
         (name-meaning
          (+brain-get-property tea-group "NAME_MEANING"))
         (name (read-string "Tea name: " name))
         (id (+brain-new-child cha--tea-parent name)))
    (org-with-point-at (org-id-find id t)
      (org-set-property "TEA_GROUP"
                        (+brain-make-link tea-group))
      (cha/set-pressing pressing)
      (+org-prompt-property "TAG")
      (org-set-property "NAME" name)
      (+org-prompt-property "NAME_ORIGINAL" name-original)
      (+org-prompt-property "NAME_TRANSCRIPTION" name-transcription)
      (+org-prompt-property "NAME_MEANING" name-meaning)
      (+org-prompt-property "YEAR_GATHERED")
      (+org-prompt-property "YEAR_MANUFACTURED")
      (places/set-dwim)
      (org-set-property "RATE" "n/a")
      (org-set-property "PRICE"
                        (concat cha-default-currency
                                (read-string "Price: ")))
      (+org-prompt-property "AVAILABLE")
      (org-set-property "TOTAL_IN" (org-entry-get nil "AVAILABLE"))
      (org-set-property "TOTAL_OUT" "0")
      (cha/pretty-tea)
      (save-buffer))))
