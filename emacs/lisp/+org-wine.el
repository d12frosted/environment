;;; +org-wine.el --- Wine tracking tool -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Nov 2019
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

(declare-function seq-contains-p "seq")
(declare-function org-edit-headline "org")
(declare-function org-entry-get "org")
(declare-function org-entry-properties "org")
(declare-function org-map-entries "org")
(declare-function org-read-date "org")
(declare-function org-set-property "org")
(declare-function org-set-tags "org")
(declare-function org-up-heading-safe "org")
(declare-function org-id-find "org-id")
(declare-function org-id-get "org-id")
(declare-function org-id-get-create "org-id")
(declare-function org-with-point-at "org-macs")
(declare-function outline-up-heading "outline")
(declare-function org-brain-add-resource "org-brain")
(declare-function org-brain-remove-parent "org-brain")

;;
;; Minor mode

;;;###autoload
(define-minor-mode wine-mode
  "Minor mode for all the wine utilities."
  :lighter " 🍷"
  (places-mode)
  (pretty-props-mode))

;;;###autoload
(defun wine-mode-maybe-enable ()
  "Conditionally enable `wine-mode' in the `org-mode' buffer.

Enables the `wine-mode' iff the buffer has 'wine-mode:t'
option set in the options section.

  #+OPTIONS: wine-mode:t"
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*wine-mode:t" (point-max) t)
        (wine-mode)))))

;;
;; Buffer settings

(def-org-buffer-prop-brain-entry
  wine-parent
  'wine-mode-hook
  "WINE_PARENT"
  "ID of wine parent entry.")

(def-org-buffer-prop
  wine-title-format
  nil
  'wine-mode-hook
  "WINE_TITLE_FORMAT"
  "Format of the wine entry title.")

(def-org-buffer-prop
  wine-rating-title-format
  nil
  'wine-mode-hook
  "WINE_RATING_TITLE_FORMAT"
  "Format of the wine entry title.")

(def-org-buffer-prop
  wine-inventory-file
  nil
  'wine-mode-hook
  "INVENTORY_FILE"
  "File name of the inventory.")

(def-org-buffer-prop-brain-entry
  wine-wineries-parent
  'wine-mode-hook
  "WINERIES_PARENT"
  "ID of wineries parent entry.")

(def-org-buffer-prop-brain-entry
  wine-regions-parent
  'wine-mode-hook
  "REGIONS_PARENT"
  "ID of regions parent entry.")

(def-org-buffer-prop-brain-entry
  wine-styles-parent
  'wine-mode-hook
  "STYLES_PARENT"
  "ID of styles parent entry.")

(def-org-buffer-prop-brain-entry
  wine-grapes-parent
  'wine-mode-hook
  "GRAPES_PARENT"
  "ID of grapes parent entry.")

(def-org-buffer-prop-list
  wine-sources
  nil
  " "
  'wine-mode-hook
  "SOURCES"
  "List of sources (or shops) of wine.")

(def-org-buffer-prop-list
  wine-colours
  nil
  ", "
  'wine-mode-hook
  "COLOURS"
  "List of colours.")

(def-org-buffer-prop-list
  wine-sweetness-levels
  nil
  ", "
  'wine-mode-hook
  "SWEETNESS"
  "List of sweetness levels.")

(def-org-buffer-prop-list
  wine-carbonation-types
  nil
  ", "
  'wine-mode-hook
  "CARBONATION"
  "List of carbonation types.")

;;
;; Grapes

(defun wine/new-grape ()
  "Create a new grape entry."
  (interactive)
  (let* ((name (read-string "Name: "))
         (id (+brain-new-child wine-grapes-parent name)))
    (org-with-point-at (org-id-find id t)
      (save-buffer)
      (pretty-props/entry)
      (save-buffer))))

(defun wine/set-grapes ()
  "Set GRAPES properties."
  (interactive)
  (+org-prompt-property-repeating
   #'+org-prompt-brain-property-fn
   "GRAPES"
   wine-grapes-parent))

;;
;; Styles

(defun wine/new-style ()
  "Create a new style entry."
  (interactive)
  (let* ((name (read-string "Name: "))
         (id (+brain-new-child wine-styles-parent name)))
    (org-with-point-at (org-id-find id t)
      (save-buffer)
      (pretty-props/entry)
      (save-buffer))))

;;
;; Regions

(defun wine/new-region ()
  "Create a new region entry."
  (interactive)
  (let* ((name (read-string "Name: "))
         (id (+brain-new-child wine-regions-parent name)))
    (org-with-point-at (org-id-find id t)
      (save-buffer)
      (places/set-dwim)
      (pretty-props/entry)
      (save-buffer))))

(defun wine/set-region ()
  "Update region of wine at point."
  (interactive)
  (let ((id (org-id-get))
        (prop (+org-entry-get "REGION")))
    (when-let* ((old-parent-id (+org-extract-id-from-link prop))
                (old-parent (+brain-as-entry old-parent-id)))
      (org-brain-remove-parent (+brain-as-entry id)
                               old-parent))
    (+org-prompt-brain-property "REGION" wine-regions-parent id 'parent)))

;;
;; Wineries

(defun wine/new-winery ()
  "Create a new winery entry."
  (interactive)
  (let* ((name (read-string "Name: "))
         (id (+brain-new-child wine-wineries-parent name)))
    (org-with-point-at (org-id-find id t)
      (save-buffer)
      (pretty-props/entry)
      (save-buffer))))

(defun wine--read-winery ()
  "Read winery."
  (+brain-choose-local-entry-by-parent
   "Winery: "
   wine-wineries-parent))

;;
;; Wine

(defun wine/new-wine ()
  "Create a new wine entry."
  (interactive)
  (let* ((winery (wine--read-winery))
         (id (+brain-new-child wine-parent (cadr winery))))
    (org-with-point-at (org-id-find id t)
      (org-set-property "WINERY" (+brain-make-link winery id 'parent))
      (+org-prompt-property "NAME")
      (org-set-property "YEAR" nil)
      (+org-prompt-brain-property "REGION" wine-regions-parent id 'parent)
      (wine/set-grapes)
      (+org-prompt-number-property "SUGAR")
      (+org-prompt-number-property "ALCOHOL")
      (+org-prompt-property "PRICE")
      (org-set-property "VOLUME" nil)
      (+org-prompt-completing-property "COLOUR" wine-colours)
      (+org-prompt-completing-property "SWEETNESS" wine-sweetness-levels)
      (+org-prompt-completing-property "CARBONATION" wine-carbonation-types)
      (while (wine--add-url (read-string "URL: ")))
      (when (y-or-n-p "Acquire? ")
        (call-interactively #'wine/acquire))
      (save-buffer)
      (wine-refresh-entry)
      (save-buffer))))

(defun wine-entry-p ()
  "Return non-nil when entry at point is wine entry."
  (string-equal (+org-parent-id) (+brain-as-id wine-parent)))

(defun wine-require-wine-entry ()
  "Throw error when entry at point is not a wine entry."
  (unless (wine-entry-p)
    (user-error "This operation is possible only on wine entry")))

(defun wine-refresh-entry ()
  "Refresh a wine entry at point."
  (wine-require-wine-entry)
  (let ((id (org-id-get-create)))
    (+org-entry-set-number "TOTAL_IN"
                           (inventory-total-in wine-inventory-file id))
    (+org-entry-set-number "TOTAL_OUT"
                           (inventory-total-out wine-inventory-file id)))
  (+org-entry-set-number "AVAILABLE"
                         (- (+org-entry-get-number "TOTAL_IN")
                            (+org-entry-get-number "TOTAL_OUT")))
  (+org-entry-set-average-number "RATE" "TOTAL" "RATING" #'wine-refresh-rating)
  (org-edit-headline
   (wine-format-title wine-title-format))
  (pretty-props/entry))

(defun wine-get-price ()
  "Get the price of wine entry at point.

Returns nil if the price is not set.

Returns first price if it's a list."
  (car (wine-get-prices)))

(defun wine-get-prices ()
  "Get prices of wine entry at point."
  (+org-entry-get-list "PRICE" ", "))

(defun wine-prompt-price ()
  "Prompt the price for wine entry at point."
  (if-let ((price (wine-get-price)))
      (let ((input (read-string (format "Price (%s): " price))))
        (if (or (null input) (string-empty-p input))
            price
          input))
    (read-string "Price: ")))

(defun wine--up-to-wine-entry ()
  "Walk up to the wine entry."
  (while (not (wine-entry-p))
    (let ((start-level (funcall outline-level)))
      (when (<= start-level 2)
        (user-error "Can't locate wine entry"))
      (outline-up-heading 1 t))))

(defmacro wine-with-point-at-wine (&rest body)
  "Move up to the wine entry and execute BODY.

Errors out when wine entry is not found. No movement is performed
when already at wine entry."
  `(save-excursion
     (wine--up-to-wine-entry)
     ,@body))

(defun wine/acquire (&optional source id amount date)
  "Acquire AMOUNT of ID because from SOURCE at DATE."
  (interactive)
  (wine-with-point-at-wine
   (let ((id (or id (org-id-get-create)))
         (source (or source (wine-read-source)))
         (amount (or amount (read-number "Amount: ")))
         (price (wine-prompt-price))
         (date (or date (org-read-date nil t))))
     (inventory-add wine-inventory-file id amount source date)
     (let ((prices (wine-get-prices)))
       (unless (seq-contains-p prices price)
         (+org-entry-set "PRICE" (+string-join (cons price prices) ", "))))
     (wine-refresh-entry))))

(defun wine/consume (&optional action id amount date)
  "Consume AMOUNT of ID because of ACTION at DATE."
  (interactive)
  (wine-with-point-at-wine
   (let ((id (or id (org-id-get-create)))
         (action (or action (read-string "Action: " "consume")))
         (amount (or amount (read-number
                             "Amount: "
                             (min (+org-entry-get-number "AVAILABLE" 1)
                                  (+org-entry-get-number "DEFAULT_AMOUNT" 1)))))
         (date (or date (org-read-date nil t))))
     (inventory-sub wine-inventory-file id amount action date)
     (when (and (string-equal action "consume")
                (y-or-n-p "Rate? "))
       (wine/rate date))
     (wine-refresh-entry))))

(defun wine-read-source ()
  "Get the source."
  (completing-read "Source: " wine-sources nil t))

;;
;; Ratings

(defvar wine--rating-props
  '(("AROMA_QUALITY" . 3)
    ("AROMA_INTENSITY" . 2)
    ("AROMA_COMPLEXITY" . 3)
    ("BALANCE" . 3)
    ("FLAVOURS" . 2)
    ("AFTERTASTE" . 3)
    ("GENERAL" . 4))
  "Wine rating properties and their max value.")

(defun wine/rate (&optional date)
  "Rate wine entry at point.

When DATE is omitted, `current-time' is used."
  (interactive)
  (wine-with-point-at-wine
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
       (+org-entry-set "DATE" (format-time-string "%Y-%m-%d" date))
       (+org-entry-set-number "RATING_VERSION" 2)
       (seq-map (lambda (cfg)
                  (+org-prompt-number-property (car cfg)
                                               0
                                               (format "0 to %i" (cdr cfg)))
                  (+org-entry-set-number (concat (car cfg) "_MAX") (cdr cfg)))
                wine--rating-props)
       (save-buffer)
       (wine-refresh-rating t)))))

(defun wine-refresh-rating (&optional propagate)
  "Refresh rating entry at point.

When PROPAGATE is non-nil, refresh is propagated upper to the
wine entry."
  (if propagate
      (save-excursion
        (org-up-heading-safe)
        (wine-refresh-entry))
    (pcase (+org-entry-get-number "RATING_VERSION" 1)
      (`2 (+org-entry-set-number
           "SCORE"
           (seq-reduce #'+
                       (seq-map #'+org-entry-get-number
                                (seq-map #'car wine--rating-props))
                       0))
          (+org-entry-set-number
           "SCORE_MAX"
           (seq-reduce #'+
                       (seq-map #'+org-entry-get-number
                                (seq-map (lambda (prop) (concat prop "_MAX"))
                                         (seq-map #'car wine--rating-props)))
                       0))
          (+org-entry-set-number
           "TOTAL"
           (* 10.0
              (/
               (float (+org-entry-get-number "SCORE"))
               (float (+org-entry-get-number "SCORE_MAX")))))))
    (org-edit-headline
     (wine-format-title wine-rating-title-format))
    (pretty-props/entry)))

;;
;; Title

(defun wine-format-title (format)
  "FORMAT the title of the wine entry at point."
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
;; URL

(defun wine/add-url ()
  "Add an URL to the wine entry at point."
  (interactive)
  (while (wine--add-url (read-string "URL: "))))

(defun wine--add-url (url)
  "Add URL to the wine entry."
  (when (and url (not (string-empty-p url)))
    (org-brain-add-resource
     url
     (or (ignore-errors (url-domain (url-generic-parse-url url)))
         url))
    t))

;;
;; Refresh

(defun wine/refresh ()
  "Refresh entry at point.

Supports the following entries:

1. Wine entry
2. Rating entry"
  (interactive)
  (cond
   ((wine-entry-p)
    (wine-refresh-entry))
   ((+org-entry-tag-p "RATING")
    (wine-refresh-rating t))
   (t
    (message "Unsupported entry"))))

(defun wine/refresh-buffer ()
  "Refresh all entries in the current buffer."
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
        (wine--refresh-mapping)))

(defun wine--refresh-mapping ()
  "Return alist of parent id and refresh function."
  (list (cons (+brain-as-id wine-parent) #'wine-refresh-entry)))

(provide '+org-wine)
;;; +org-wine.el ends here
