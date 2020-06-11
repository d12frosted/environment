;;; +org-cigars.el --- Cigar tracking tool -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Nov 2019
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
(require '+org-pretty-props)
(require '+org-prop)
(require '+org-places)
(require '+org-buffer-prop)
(require '+inventory)

;;
;; Minor mode

;;;###autoload
(define-minor-mode cigars-mode
  "Minor mode for all the cigars utilities."
  :lighter " 🚬"
  (places-mode)
  (pretty-props-mode))

;;;###autoload
(defun cigars-mode-maybe-enable ()
  "Conditionally enable `cigars-mode' in the `org-mode' buffer.

Enables the `cigars-mode' iff the buffer has 'cigars-mode:t'
option set in the options section.

  #+OPTIONS: cigars-mode:t"
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*cigars-mode:t" (point-max) t)
        (cigars-mode)))))

;;
;; Buffer settings

(def-org-buffer-prop-brain-entry
  cigars-parent
  'cigars-mode-hook
  "CIGARS_PARENT"
  "ID of cigars parent entry.")

(def-org-buffer-prop-brain-entry
  cigars-brands-parent
  'cigars-mode-hook
  "BRANDS_PARENT"
  "ID of brands parent entry.")

(def-org-buffer-prop-brain-entry
  cigars-materials-location-parent
  'cigars-mode-hook
  "MATERIALS_LOCATION_PARENT"
  "ID of materials location parent entry.")

(def-org-buffer-prop
  cigar-title-format
  nil
  'cigars-mode-hook
  "CIGAR_TITLE_FORMAT"
  "Format of the cigar entry title.")

(def-org-buffer-prop
  cigar-rating-title-format
  nil
  'cigars-mode-hook
  "RATING_TITLE_FORMAT"
  "Format of the rating entry title.")

(def-org-buffer-prop
  cigars-inventory-file
  nil
  'cigars-mode-hook
  "INVENTORY_FILE"
  "File name of the inventory.")

(def-org-buffer-prop-list
  cigars-sources
  nil
  " "
  'cigars-mode-hook
  "SOURCES"
  "List of sources (or shops) of cigars.")

;;
;; Refresh

(defun cigar/refresh ()
  "Refresh entry at point.

Supports the following entries:

1. Cigar entry
2. Rating entry"
  (interactive)
  (cond
   ((string-equal (+org-parent-id) (+brain-as-id cigars-parent))
    (cigar-refresh-entry))
   ((+org-entry-tag-p "RATING")
    (cigar-refresh-rating t))
   (t
    (message "Unsupported entry"))))

(defun cigar/refresh-buffer ()
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
        (cigar--refresh-mapping)))

(defun cigar--refresh-mapping ()
  "Return alist of parent id and refresh function."
  (list (cons (+brain-as-id cigars-parent) #'cigar-refresh-entry)))

(defun cigar-refresh-entry ()
  "Refresh a cigar entry at point."
  (let ((id (org-id-get-create)))
    (+org-entry-set-number "TOTAL_IN"
                           (inventory-total-in cigars-inventory-file id))
    (+org-entry-set-number "TOTAL_OUT"
                           (inventory-total-out cigars-inventory-file id)))
  (+org-entry-set-number "AVAILABLE"
                         (round (- (+org-entry-get-number "TOTAL_IN")
                                   (+org-entry-get-number "TOTAL_OUT"))))
  (+org-entry-set-average-number "RATE" "TOTAL" "RATING" #'cigar-refresh-rating)
  (+org-entry-set-average-number "DURATION" "DURATION" "RATING")
  (+org-entry-set-average-number "STRENGTH" "STRENGTH" "RATING")
  (org-edit-headline
   (cigar-format-title cigar-title-format))
  (pretty-props/entry))

(defun cigar-refresh-rating (&optional propagate)
  "Refresh rating entry at point.

When PROPAGATE is non-nil, refresh is propagated upper to the
cigar entry."
  (if propagate
    (save-excursion
      (org-up-heading-safe)
      (cigar-refresh-entry))
    (org-edit-headline
     (cigar-format-title cigar-rating-title-format))
    (pretty-props/entry)))

;;
;; Title

(defun cigar-format-title (format)
  "FORMAT the title of the cigar entry at point."
  (let ((result format)
        (properties (org-entry-properties))
        (prop-regexp "{\\([a-zA-Z0-9\\-_]+\\):?\\([%\\.0-9A-Za-z]*\\)"))
    (while (string-match prop-regexp result)
      (let* ((prop (match-string 1 result))
             (val-maybe-format (match-string 2 result))
             (val-format (if (string-empty-p val-maybe-format) "%s" val-maybe-format))
             (val-format-regexp
              (unless (string-empty-p val-maybe-format)
                (concat ":" val-maybe-format)))
             (val-original (cdr (assoc-string prop properties)))
             (val-def
              (if (or (string-suffix-p "f" val-format)
                      (string-suffix-p "d" val-format))
                  "0"
                "n/a"))
             (val (replace-regexp-in-string
                   "\\[\\[.+\\]\\[\\(.+\\)\\]\\]"
                   "\\1"
                   (or val-original val-def)))
             (val-conv
              (if (or (string-suffix-p "f" val-format)
                      (string-suffix-p "d" val-format))
                  (string-to-number val)
                val)))
        (setq result (replace-regexp-in-string
                      (concat "{" prop val-format-regexp "}")
                      (or (format val-format val-conv) "?")
                      result
                      t))))
    result))

;;
;; Cigars

(defun cigar/new ()
  "Create a new cigar entry."
  (interactive)
  (let* ((brand (cigars--read-brand))
         (id (+brain-new-child cigars-parent (cadr brand))))
    (org-with-point-at (org-id-find id t)
      (org-set-property "BRAND" (+brain-make-link brand))
      (+org-prompt-property "NAME")
      (+org-prompt-property "RING_GAUGE")
      (+org-prompt-property "LENGTH")
      (+org-prompt-property "SHAPE")
      (+org-prompt-brain-property "WRAPPER" cigars-materials-location-parent)
      (+org-prompt-brain-property "BINDER" cigars-materials-location-parent)
      (+org-prompt-brain-property "FILLER" cigars-materials-location-parent)
      (+org-prompt-property "PRICE")
      (+org-prompt-property "AVAILABLE")
      (inventory-add cigars-inventory-file
                     id
                     (org-entry-get nil "AVAILABLE")
                     (cigar-read-source)
                     (org-read-date nil t nil "Date of purchase: "))
      (org-set-property "TOTAL_IN" (org-entry-get nil "AVAILABLE"))
      (org-set-property "TOTAL_OUT" "0")
      (cigar-refresh-entry)
      (save-buffer)
      (cigar-refresh-entry)
      (save-buffer))))

(defun cigar-entry-p ()
  "Return non-nil when entry at point is cigar entry."
  (string-equal (+org-parent-id) (+brain-as-id cigars-parent)))

(defun cigar-get-price ()
  "Get the price of cigar entry at point.

Returns nil if the price is not set.

Returns first price if it's a list."
  (car (cigar-get-prices)))

(defun cigar-get-prices ()
  "Get prices of cigar entry at point."
  (+org-entry-get-list "PRICE" ", "))

(defun cigar-prompt-price ()
  "Prompt the price for cigar entry at point."
  (if-let ((price (cigar-get-price)))
      (let ((input (read-string (format "Price (%s): " price))))
        (if (or (null input) (string-empty-p input))
            price
          input))
    (read-string "Price: ")))

(defun cigar--up-to-entry ()
  "Walk up to the cigar entry."
  (while (not (cigar-entry-p))
    (let ((start-level (funcall outline-level)))
      (when (<= start-level 2)
        (user-error "Can't locate cigar entry"))
      (outline-up-heading 1 t))))

(defmacro cigar-with-point-at-entry (&rest body)
  "Move up to the cigar entry and execute BODY.

Errors out when cigar entry is not found. No movement is performed
when already at cigar entry."
  `(save-excursion
     (cigar--up-to-entry)
     ,@body))

(defun cigar/acquire (&optional source id amount date)
  "Acquire AMOUNT of ID because from SOURCE at DATE."
  (interactive)
  (cigar-with-point-at-entry
   (let ((id (or id (org-id-get-create)))
         (source (or source (cigar-read-source)))
         (amount (or amount (read-number "Amount: ")))
         (price (cigar-prompt-price))
         (date (or date (org-read-date nil t))))
     (inventory-add cigars-inventory-file id amount source date)
     (let ((prices (cigar-get-prices)))
       (unless (seq-contains prices price)
         (+org-entry-set "PRICE" (+string-join (cons price prices) ", "))))
     (cigar-refresh-entry))))

(defun cigar-read-source ()
  "Get the source."
  (completing-read "Source: " cigars-sources nil t))

(defun cigar/consume (&optional action id amount date)
  "Consume AMOUNT of ID because of ACTION at DATE."
  (interactive)
  (let ((id (or id (org-id-get-create)))
        (action (or action (read-string "Action: " "consume")))
        (amount (or amount (read-number
                            "Amount: "
                            (+org-entry-get-number "DEFAULT_AMOUNT" 1))))
        (date (or date (org-read-date nil t))))
    (inventory-sub cigars-inventory-file id amount action date)
    (when (and (string-equal action "consume")
               (y-or-n-p "Rate? "))
      (cigar/rate date))
    (cigar-refresh-entry)))

(defun cigar/rate (&optional date)
  "Rate cigar entry at point.

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
      (+org-prompt-number-property "TOTAL")
      (+org-prompt-number-property "DURATION")
      (+org-prompt-number-property "STRENGTH")
      (save-buffer)
      (cigar-refresh-rating t))))

;;
;; Brands

(defun cigars--read-brand ()
  "Read Brand."
  (+brain-choose-entry-by-parent
   "Brand: "
   cigars-brands-parent))

(defun cigar/new-brand ()
  "Create a new brand entry."
  (interactive)
  (let* ((name (read-string "Name: "))
         (id (+brain-new-child cigars-brands-parent name)))
    (org-with-point-at (org-id-find id t)
      (places/set-dwim)
      (save-buffer)
      (pretty-props/entry)
      (save-buffer))))

(provide '+org-cigars)
;;; +org-cigars.el ends here
