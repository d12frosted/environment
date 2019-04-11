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
;; Refresh

(defun cha--refresh-mapping ()
  "Return alist of parent id and refresh function."
  (list (cons cha-tea-groups-parent-id #'cha-refresh-tea-group-entry)
        (cons cha-tea-parent-id #'cha-refresh-tea-entry)))

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

Support following entries:

1. Tea group entry
2. Tea entry
3. Rating entry"
  (interactive)
  (cond
   ((string-equal (+org-parent-id) cha-tea-groups-parent-id)
    (cha-refresh-tea-group-entry))
   ((string-equal (+org-parent-id) cha-tea-parent-id)
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
    (+org-entry-set-number "TOTAL_IN" (cha-inv--total-in id))
    (+org-entry-set-number "TOTAL_OUT" (cha-inv--total-out id)))
  (+org-entry-set-number "AVAILABLE"
                         (round (- (+org-entry-get-number "TOTAL_IN")
                                   (+org-entry-get-number "TOTAL_OUT"))))
  (let ((rates (org-map-entries
                (lambda ()
                  (cha-refresh-tea-rating nil)
                  (+org-entry-get-number "TOTAL"))
                "+RATING"
                'tree)))
    (+org-entry-set-number "RATE"
                           (if (null rates)
                               0
                             (/ (apply #'+ rates)
                                (float (length rates))))))
  (org-edit-headline
   (cha-format-tea-title cha-tea-title-format))
  (pretty-props/entry))

(defun cha-refresh-tea-rating (propagate)
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
      (cha-inv--add id
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
    (cha-inv--sub id amount action date)
    (when (and (string-equal action "drink")
               (y-or-n-p "Rate?"))
      (cha/rate date))
    (cha-refresh-tea-entry)))

(defun cha/acquire (&optional source id amount date)
  "Acquire AMOUNT of ID because from SOURCE at DATE."
  (interactive)
  (let ((id (or id (org-id-get-create)))
        (source (or source (read-string "Source: " "belayasova")))
        (amount (or amount (read-number "Amount: ")))
        (date (or date (org-read-date nil t))))
    (cha-inv--add id amount source date)
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
      (org-set-tags-to ":RATING:")
      (org-set-property "DATE" (format-time-string "%Y-%m-%d" date))
      (mapc #'+org-prompt-property cha--tea-rating-props)
      (save-buffer)
      (cha-refresh-tea-rating t))))

;;
;; Inventory

(defun cha-inv--balance (id &optional query)
  "Get balance of ID using QUERY."
  (let* ((cmd (format "hledger -f cha-dao.journal b %s '%s'" id query))
         (res (shell-command-to-string cmd))
         (lines (split-string res "\n")))
    (string-to-number (car (seq-drop-while #'string-empty-p (reverse lines))))))

(defun cha-inv--total-in (id)
  "Get total income for ID."
  (cha-inv--balance id "amt:>0"))

(defun cha-inv--total-out (id)
  "Get total outcome for ID."
  (abs (cha-inv--balance id "amt:<0")))

(defun cha-inv--add (id amount source &optional date)
  "Add AMOUNT of ID to inventory from SOURCE.

When DATE is omitted, `current-time' is used."
  (shell-command-to-string
   (format
    "echo '\n%s\n    cha:%s  %s\n    source:%s' >> cha-dao.journal"
    (format-time-string "%Y/%m/%d" date)
    id
    amount
    source)))

(defun cha-inv--sub (id amount action &optional date)
  "Subtract amount of ID from inventory as result of ACTION.

When DATE is omitted, `current-time' is used."
  (shell-command-to-string
   (format
    "echo '\n%s\n    activity:%s  %s\n    cha:%s' >> cha-dao.journal"
    (format-time-string "%Y/%m/%d" date)
    action
    amount
    id)))
