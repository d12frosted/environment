;;; +org-cha.el --- Tea tracking tool -*- lexical-binding: t; -*-
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

(require 'map)
(require '+org)
(require '+org-pretty-props)
(require '+org-prop)
(require '+org-places)
(require '+org-buffer-prop)
(require '+inventory)

;;;###autoload
(define-minor-mode cha-mode
  "Minor mode for all the tea utilities."
  :lighter " 茶"
  (places-mode)
  (pretty-props-mode))

;;
;; Buffer settings

(def-org-buffer-prop-brain-entry
  cha-tea-groups-parent
  'cha-mode-hook
  "TEA_GROUPS_PARENT"
  "ID of tea groups parent entry.")

(def-org-buffer-prop-brain-entry
  cha-tea-parent
  'cha-mode-hook
  "TEA_PARENT"
  "ID of tea parent entry.")

(def-org-buffer-prop-brain-entry
  cha-fermentation-types-parent
  'cha-mode-hook
  "FERMENTATION_TYPES_PARENT"
  "ID of fermentation types parent entry")

(def-org-buffer-prop-brain-entry
  cha-pressing-types-parent
  'cha-mode-hook
  "PRESSING_TYPES_PARENT"
  "ID of pressing types parent entry")

(def-org-buffer-prop
  cha-tea-title-format
  nil
  'cha-mode-hook
  "TEA_TITLE_FORMAT"
  "Format of the tea entry title.")

(def-org-buffer-prop
  cha-default-currency
  ""
  'cha-mode-hook
  "DEFAULT_CURRENCY"
  "Default currency.")

(def-org-buffer-prop
  cha-inventory-file
  ""
  'cha-mode-hook
  "INVENTORY_FILE"
  "File name of the inventory.")

;;
;; Refresh

(defun cha--refresh-mapping ()
  "Return alist of parent id and refresh function."
  (list (cons (+brain-as-id cha-tea-groups-parent) #'cha-refresh-tea-group-entry)
        (cons (+brain-as-id cha-tea-parent) #'cha-refresh-tea-entry)))

(defun cha/refresh-buffer ()
  "Refresh all entries in the current buffer.."
  (interactive)
  (mapc (lambda (config)
          (let* ((parent-id (car config))
                 (refreshf (cdr config))
                 (loc (org-id-find parent-id)))
            (+org-with-file
             (car loc)
             (org-with-point-at (cdr loc)
               (org-map-entries (lambda ()
                                  (cond
                                   ((string-equal (org-id-get) parent-id)
                                    (pretty-props/entry))
                                   ((string-equal (+org-parent-id) parent-id)
                                    (funcall refreshf))
                                   (t)))
                                nil 'tree)))))
        (cha--refresh-mapping)))

(defun cha/refresh ()
  "Refresh entry at point.

Supports the following entries:

1. Tea group entry
2. Tea entry
3. Rating entry"
  (interactive)
  (cond
   ((string-equal (+org-parent-id) (+brain-as-id cha-tea-groups-parent))
    (cha-refresh-tea-group-entry))
   ((string-equal (+org-parent-id) (+brain-as-id cha-tea-parent))
    (cha-refresh-tea-entry))
   ((+org-entry-tag-p "RATING")
    (cha-refresh-tea-rating t))
   (t
    (message "Unsupported entry"))))

(defun cha-refresh-tea-group-entry ()
  "Refresh tea group entry at point."
  (pretty-props/entry))

(defun cha-refresh-tea-entry ()
  "Refresh tea entry at point."
  (let ((id (org-id-get-create)))
    (+org-entry-set-number "TOTAL_IN"
                           (inventory-total-in cha-inventory-file id))
    (+org-entry-set-number "TOTAL_OUT"
                           (inventory-total-out cha-inventory-file id)))
  (+org-entry-set-number "AVAILABLE"
                         (round (- (+org-entry-get-number "TOTAL_IN")
                                   (+org-entry-get-number "TOTAL_OUT"))))
  (+org-entry-set-average-number "RATE" "TOTAL" "RATING" #'cha-refresh-tea-rating)
  (org-edit-headline
   (cha-format-tea-title cha-tea-title-format))
  (pretty-props/entry))

(defun cha-refresh-tea-rating (&optional propagate)
  "Refresh tea rating entry at point.

When PROPAGATE is non-nil, refresh is propagated upper to the tea
entry."
  (if propagate
    (save-excursion
      (org-up-heading-safe)
      (cha-refresh-tea-entry))
    (+org-entry-set-number "TOTAL" (cha--tea-rating))
    (pretty-props/entry)))

(defvar cha--tea-rating-props
  '("DRY_LEAF_APPEARANCE"
    "DRY_LEAF_AROMA"
    "WARM_LEAF_AROMA"
    "BREWED_APPEARANCE"
    "BREWED_AROMA"
    "BREWED_FLAVOR"
    "DEVELOPMENT"
    "BOTTOM"
    "AFTER_STATE"
    "GENERAL")
  "Tea rating properties.")

(defun cha--tea-rating ()
  "Get rating value from tea rating entry at point."
  (/ (seq-reduce #'+ (seq-map #'+org-entry-get-number cha--tea-rating-props) 0)
     (float (length cha--tea-rating-props))))

;;
;; Title

(defun cha-format-tea-title (format)
  "FORMAT a tea entry title."
  (let ((properties (org-entry-properties))
        (val-regexp "{\\([a-zA-Z0-9\\-_]+\\)")
        (prop "")
        (val "")
        (result format))
    (setf (map-elt properties "LOCATION") (places-get-location))
    (setf (map-elt properties "YEAR") (cha-get-year))
    (while (string-match val-regexp result)
      (setq prop (match-string 1 result))
      (setq val (cdr (assoc-string prop properties)))
      (setq result (replace-regexp-in-string
                    (concat "{" prop "}")
                    (or val "?")
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
;; Tea groups

(defun cha--read-tea-group ()
  "Read Tea Group."
  (+brain-choose-entry-by-parent
   "Tea group: "
   cha-tea-groups-parent))

(defun cha/new-tea-group ()
  "Create a new tea group entry."
  (interactive)
  (let* ((name (read-string "Tea group name: "))
         (id (+brain-new-child cha-tea-groups-parent name)))
    (org-with-point-at (org-id-find id t)
      (+org-prompt-property "NAME_ORIGINAL")
      (+org-prompt-property "NAME_TRANSCRIPTION")
      (+org-prompt-property "NAME_MEANING")
      (+org-prompt-brain-property "FERMENTATION"
                                  cha-fermentation-types-parent)
      (pretty-props/entry)
      (save-buffer))))

;;
;; Pressing

(defun cha--read-pressing ()
  "Read Tea Group."
  (+brain-choose-entry-by-parent
   "Pressing: "
   cha-pressing-types-parent))

(defun cha/set-pressing (&optional pressing)
  "Set PRESSING of tea entry at point."
  (interactive)
  (unless pressing
    (setq pressing (cha--read-pressing)))
  (org-set-property "PRESSING"
                    (+brain-make-link pressing)))

;;
;; Tea

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
         (id (+brain-new-child cha-tea-parent name)))
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
      (inventory-add cha-inventory-file
                     id
                     (org-entry-get nil "AVAILABLE")
                     (read-string "Shop:" "belayasova")
                     (org-read-date nil t nil "Date of purchase: "))
      (org-set-property "TOTAL_IN" (org-entry-get nil "AVAILABLE"))
      (org-set-property "TOTAL_OUT" "0")
      (cha-refresh-tea-entry)
      (save-buffer))))

(defun cha/drink ()
  "Drink tea at point."
  (interactive)
  (cha/consume "drink"))

(defun cha/consume (&optional action id amount date)
  "Consume AMOUNT of ID because of ACTION at DATE."
  (interactive)
  (let ((id (or id (org-id-get-create)))
        (action (or action (read-string "Action: " "drink")))
        (amount (or amount (read-number
                            "Amount: "
                            (+org-entry-get-number "DEFAULT_AMOUNT"))))
        (date (or date (org-read-date nil t))))
    (inventory-sub cha-inventory-file id amount action date)
    (when (and (string-equal action "drink")
               (y-or-n-p "Rate? "))
      (cha/rate date))
    (cha-refresh-tea-entry)))

(defun cha/acquire (&optional source id amount date)
  "Acquire AMOUNT of ID because from SOURCE at DATE."
  (interactive)
  (let ((id (or id (org-id-get-create)))
        (source (or source (read-string "Source: " "belayasova")))
        (amount (or amount (read-number "Amount: ")))
        (date (or date (org-read-date nil t))))
    (inventory-add cha-inventory-file id amount source date)
    (cha-refresh-tea-entry)))

(defun cha/rate (&optional date)
  "Rate tea entry at point.

When DATE is omitted, `current-time' is used."
  (interactive)
  (let* ((date (or date (org-read-date nil t)))
         (name (concat
                (format-time-string "%Y-%m-%d %A" date)
                " | "
                (org-entry-get nil "NAME")
                " "
                (org-entry-get nil "TAG")))
         (id (+brain-new-child (org-id-get-create) name)))
    (org-with-point-at (org-id-find id t)
      (org-set-tags ":RATING:")
      (org-set-property "DATE" (format-time-string "%Y-%m-%d" date))
      (mapc #'+org-prompt-property cha--tea-rating-props)
      (save-buffer)
      (cha-refresh-tea-rating t))))

;;
;; Migration

;;;###autoload
(defun cha-migrate/new-tea (tea-group-id
                            name
                            name-original
                            name-transcription
                            name-meaning
                            tag
                            pressing-id
                            year-gathered
                            year-manufactured
                            price)
  "Migration function.

I don't care about documentation string here, so let me just
mention all the arguments, so my linter doesn't yell at me:

- TEA-GROUP-ID
- NAME
- NAME-ORIGINAL
- NAME-TRANSCRIPTION
- NAME-MEANING
- TAG
- PRESSING-ID
- YEAR-GATHERED
- YEAR-MANUFACTURED
- PRICE"
  (let* ((tea-group (+brain-as-entry tea-group-id))
         (pressing (+brain-as-entry pressing-id))
         (name (or name (cadr tea-group)))
         (name-original
          (or name-original
              (+brain-get-property tea-group "NAME_ORIGINAL")))
         (name-transcription
          (or name-transcription
              (+brain-get-property tea-group "NAME_TRANSCRIPTION")))
         (name-meaning
          (or name-meaning
              (+brain-get-property tea-group "NAME_MEANING")))
         (id (+brain-new-child cha-tea-parent name)))
    (org-with-point-at (org-id-find id t)
      (org-set-property "TEA_GROUP" (+brain-make-link tea-group))
      (org-set-property "PRESSING" (+brain-make-link pressing))
      (org-set-property "TAG" tag)
      (org-set-property "NAME" name)
      (org-set-property "NAME_ORIGINAL" name-original)
      (org-set-property "NAME_TRANSCRIPTION" name-transcription)
      (org-set-property "NAME_MEANING" name-meaning)
      (org-set-property "YEAR_GATHERED" year-gathered)
      (org-set-property "YEAR_MANUFACTURED" year-manufactured)
      ;; (places/set-dwim) ;; TODO: change it
      (org-set-property "RATE" "n/a")
      (org-set-property "PRICE" (concat cha-default-currency price))
      (+org-prompt-property "AVAILABLE")
      (save-buffer)
      (cha-refresh-tea-entry)
      (save-buffer))))

(provide '+org-cha)
;;; +org-cha.el ends here
