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
(require 'wine)

(autoload 'seq-contains-p "seq")
(autoload 'org-edit-headline "org")
(autoload 'org-entry-get "org")
(autoload 'org-entry-properties "org")
(autoload 'org-map-entries "org")
(autoload 'org-read-date "org")
(autoload 'org-set-property "org")
(autoload 'org-set-tags "org")
(autoload 'org-up-heading-safe "org")
(autoload 'org-id-find "org-id")
(autoload 'org-id-get "org-id")
(autoload 'org-id-get-create "org-id")
(autoload 'org-with-point-at "org-macs")
(autoload 'outline-up-heading "outline")
(autoload 'org-brain-add-resource "org-brain")
(autoload 'org-brain-remove-parent "org-brain")

;;
;; Minor mode

;;;###autoload
(define-minor-mode wine-mode
  "Minor mode for all the wine utilities."
  :lighter " 🍷"
  (places-mode)
  (pretty-props-mode))

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
   (lambda (_)
     (let ((grape (wine-grape-select)))
       (org-make-link-string
        (concat "id:" (plist-get grape :id))
        (plist-get grape :title))))
   "GRAPES"))

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
  (when-let* ((id (org-id-get))
              (prop (+org-entry-get "REGION"))
              (old-parent-id (+org-extract-id-from-link prop))
              (old-parent (+brain-as-entry old-parent-id)))
    (org-brain-remove-parent (+brain-as-entry id)
                             old-parent))
  (when-let* ((region (wine-region-select)))
    (+org-entry-set "APPELLATION"
                    (org-make-link-string
                     (concat "id:" (plist-get region :id))
                     (plist-get region :title)))))

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

;;
;; Wine

(defun wine/new-wine ()
  "Create a new wine entry."
  (interactive)
  (let* ((winery (wine-producer-select))
         (id (+brain-new-child wine-parent (cadr winery))))
    (org-with-point-at (org-id-find id t)
      (org-set-property "WINERY" (org-make-link-string
                                  (concat "id:" (plist-get winery :id))
                                  (plist-get winery :title)))
      (+org-prompt-property "NAME")
      (org-set-property "YEAR" nil)
      (when-let* ((region (wine-region-select)))
        (+org-entry-set "APPELLATION"
                        (org-make-link-string
                         (concat "id:" (plist-get region :id))
                         (plist-get region :title))))
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

(defun wine/copy-wine ()
  "Create a new wine entry based on wine entry at point."
  (interactive)
  (wine-require-wine-entry)
  (let* ((winery (+brain-as-entry (+org-entry-get-brain "WINERY")))
         (name (+org-entry-get "NAME"))
         (region (+org-entry-get-brain "REGION"))
         (grapes (+org-entry-get "GRAPES"))
         (volume (+org-entry-get "VOLUME"))
         (colour (+org-entry-get "COLOUR"))
         (carbonation (+org-entry-get "CARBONATION"))
         (id (+brain-new-child wine-parent (cadr winery))))
    (org-with-point-at (org-id-find id t)
      (+org-entry-set "WINERY" (+brain-make-link winery id 'parent))
      (+org-entry-set "NAME" name)
      (+org-entry-set "REGION" (+brain-make-link region id 'parent))
      (+org-entry-set "GRAPES" grapes)
      (+org-entry-set "VOLUME" volume)
      (+org-entry-set "COLOUR" colour)
      (+org-entry-set "CARBONATION" carbonation)
      (+org-prompt-property "YEAR")
      (+org-prompt-number-property "SUGAR")
      (+org-prompt-number-property "ALCOHOL")
      (+org-prompt-property "PRICE")
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

(defvar wine--rating-props-v2
  '(("AROMA_QUALITY" . 3)
    ("AROMA_INTENSITY" . 2)
    ("AROMA_COMPLEXITY" . 3)
    ("BALANCE" . 3)
    ("FLAVOURS" . 2)
    ("AFTERTASTE" . 3)
    ("GENERAL" . 4))
  "Wine rating properties and their max value.")

(defvar wine--rating-props-v3
  '(("AROMA_QUALITY" .
     (lambda ()
       (let* ((total 3)
              (res total)
              (ans t)
              (quit-on "no taints")
              (opts (list
                     quit-on
                     "aggressive ethanol"
                     "massive brett attack"
                     "VA, especially nail polish removal")))
         (while ans
           (setq ans (completing-read "Any taints? " opts))
           (setq opts (delete ans opts))
           (if (string-equal ans "no taints")
               (setq ans nil)
             (setq res (max 0 (- res 1))))
           (when (equal res 0)
             (setq ans nil)))
         (cons res total))))

    ("AROMA_INTENSITY" .
     (("aroma can be perceived without putting nose into glass" . 2)
      ("aroma can be perceived only by putting nose into glass" . 1)
      ("closed, you need to put a lot of effort to get the aroma" . 0)))

    ("AROMA_RICHNESS" .
     (("more than 3 different notes" . 3)
      ("only 3 notes" . 2)
      ("only 2 notes" . 1)
      ("only 1 note" . 0)))

    ("AROMA_COMPLEXITY" .
     (("sophisticated, multilayered" . 1)
      ("simple" . 0)))

    ("BALANCE" .
     (("perfectly balanced, everything is in its place" . 3)
      ("well balanced, might be a small issue" . 2)
      ("average, either one bigger issue or two small" . 1)
      ("unbalanced, everything else" . 0)))

    ("FLAVOURS" .
     (("multiple flavours" . 1)
      ("only one flavour" . 0)))

    ("EVOLUTION" .
     (("taste and flavours evolve over time in mouth" . 1)
      ("plain, straightforward" . 0)))

    ("AFTERTASTE" .
     (("long, lasting more than 30 seconds" . 2)
      ("average, lasting more than 10 seconds" . 1)
      ("short" . 0)))

    ("GENERAL" .
     (("life changing" . 4)
      ("great wine, I will definitely look into tasting it once more" . 3)
      ("good wine, will drink it again with pleasure if situation arises" . 2)
      ("average wine, only with parents" . 1)
      ("bad wine, only for enemies" . 0))))
  "Wine rating properties and possible values.")

(defun wine/rate (&optional date)
  "Rate wine entry at point.

When DATE is omitted, date is read using `org-read-date'."
  (interactive)
  (let ((date (or date (org-read-date nil t))))
    (wine-rate-v3 (format-time-string "%Y-%m-%d" date) date)))

(defun wine-rate-v2 (name date)
  "Rate a wine using Rating System V2.

See `wine-rate-g` for more information on NAME and DATE usage."
  (wine-rate-generic name date 2 wine--rating-props-v2))

(defun wine-rate-v3 (name date)
  "Rate a wine using Rating System V3.

See `wine-rate-g` for more information on NAME and DATE usage."
  (wine-rate-generic name date 3 wine--rating-props-v3))

(defun wine-rate-generic (name date version props)
  "Rate a wine at point on DATE.

The procedure is simple:

1. Create a new child with NAME and 'RATING' tag.
2. Set the DATE as property 'DATE'.
3. Set the VERSION as property 'RATING_VERSION'.
4. Query rating marks based on PROPS."
  (wine-with-point-at-wine
   (let* ((id (+brain-new-child (org-id-get-create) name)))
     (org-with-point-at (org-id-find id t)
       (org-set-tags ":RATING:")
       (+org-entry-set "DATE" (format-time-string "%Y-%m-%d" date))
       (+org-entry-set-number "RATING_VERSION" version)
       (seq-map
        (lambda (cfg)
          (cond
           ((functionp (cdr cfg))
            (let ((res (funcall (cdr cfg))))
              (+org-entry-set-number (car cfg) (car res))
              (+org-entry-set-number (concat (car cfg) "_MAX") (cdr res))))

           ((numberp (cdr cfg))
            (+org-prompt-number-property (car cfg) 0 (format "0 to %i" (cdr cfg)))
            (+org-entry-set-number (concat (car cfg) "_MAX") (cdr cfg)))

           ((listp (cdr cfg))
            (let* ((ans (completing-read (+org--pretty-property-prompt (car cfg))
                                         (seq-map #'car (cdr cfg))))
                   (res (assoc ans (cdr cfg))))
              (+org-entry-set-number (car cfg) (cdr res))
              (+org-entry-set-number (concat (car cfg) "_MAX") (- (length (cdr cfg)) 1)))
            )))
        props)
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
      (`2 (wine-refresh-rating-by wine--rating-props-v2))
      (`3 (wine-refresh-rating-by wine--rating-props-v3)))
    (org-edit-headline
     (wine-format-title wine-rating-title-format))
    (pretty-props/entry)))

(defun wine-refresh-rating-by (props)
  "Refresh rating entry at point based on PROPS."
  (+org-entry-set-number
   "SCORE"
   (seq-reduce #'+
               (seq-map #'+org-entry-get-number
                        (seq-map #'car props))
               0))
  (+org-entry-set-number
   "SCORE_MAX"
   (seq-reduce #'+
               (seq-map #'+org-entry-get-number
                        (seq-map (lambda (prop) (concat prop "_MAX"))
                                 (seq-map #'car props)))
               0))
  (+org-entry-set-number
   "TOTAL"
   (* 10.0
      (/
       (float (+org-entry-get-number "SCORE"))
       (float (+org-entry-get-number "SCORE_MAX"))))))

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
  (wine-with-point-at-wine
   (while (wine--add-url (read-string "URL: ")))))

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

;;
;; Migration

(defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (org-remove-file file)
	(throw 'nextfile t))

(defun wine-migrate--find-note (title)
  "Find a note with TITLE."
  (car (+seq-flatten
        (org-roam-db-query [:select file
                            :from titles
                            :where (= title $s1)]
                           title))))

(defun wine-migrate--find-country (region-entry-or-id)
  "Find a country from REGION-ENTRY-OR-ID."
  (let* ((entry (+brain-as-entry region-entry-or-id))
         (parents (org-brain-parents entry))
         (countries-id "ec4a5bd7-71c4-479a-8bbb-8f022e78f52d"))
    (if (seq-find (lambda (x)
                    (string-equal countries-id (+brain-as-id x)))
                  parents)
        entry
      (seq-find (lambda (x)
                  (seq-find (lambda (y)
                              (string-equal countries-id (+brain-as-id y)))
                            (org-brain-parents x)))
                parents))))

(defun wine-migrate--ensure-appellation (entry-or-id)
  "Ensure that appellation described by ENTRY-OR-ID exists.

Transitively ensures that parent region and country both exist.

Return (:country-file :country-name
        :region-file :region-name
        :appellation-file :appellation-name)."
  (let* ((entry (+brain-as-entry entry-or-id))
         (id (+brain-as-id entry-or-id))
         (name (+brain-title entry))
         (file (wine-migrate--find-note name))
         (country-id (let ((f (org-id-find-id-file id)))
                       (+org-with-file  f
                         (goto-char (cdr (org-id-find-id-in-file id f)))
                         (+org-entry-get-brain "COUNTRY"))))
         (region-id (unless country-id
                      (let ((f (org-id-find-id-file id)))
                        (+org-with-file  f
                          (goto-char (cdr (org-id-find-id-in-file id f)))
                          (or (+org-entry-get-brain "REGION")
                              (+seq-singleton (+org-entry-get-list "BRAIN_PARENTS" " ")))))))
         (result (if country-id
                     (wine-migrate--ensure-country country-id)
                   (wine-migrate--ensure-region region-id))))
    (unless file
      (let* ((org-roam-capture-immediate-template-old org-roam-capture-immediate-template)
             (org-roam-capture-immediate-template `("d" "default" plain
                                                    #'org-roam-capture--get-point
                                                    "%?"
                                                    :file-name "wine/appellation/%<%Y%m%d%H%M%S>-${slug}"
                                                    :head ,(concat
                                                            "#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
                                                            "- country :: "
                                                            (org-roam-format-link (plist-get result :country-file)
                                                                                  (plist-get result :country-name))
                                                            "\n"
                                                            (when (plist-get result :region-file)
                                                              (concat "- region :: "
                                                                      (org-roam-format-link (plist-get result :region-file)
                                                                                            (plist-get result :region-name))
                                                                      "\n"))
                                                            "- resources :: "
                                                            (let ((url (read-string "URL: ")))
                                                              (org-link-make-string
                                                               url
                                                               (or (ignore-errors (url-domain (url-generic-parse-url url)))
                                                                   (read-string "Description: "))))
                                                            "\n")
                                                    :unnarrowed t
                                                    :immediate-finish t)))
        (org-roam-find-file-immediate name nil nil t)
        (org-roam-db-build-cache)
        (setq org-roam-capture-immediate-template org-roam-capture-immediate-template-old)))
    (plist-put
     (plist-put
      result
      :appellation-file (wine-migrate--find-note name))
     :appellation-name name)))

(defun wine-migrate--ensure-region (region-entry-or-id)
  "Ensure that region described by REGION-ENTRY-OR-ID exists.

Transitively ensures that country exists.

Return (:country-file :country-name :region-file :region-name)."
  (when (null region-entry-or-id)
    (user-error "REGION-ENTRY-OR-ID must be a valid string"))
  (let* ((result (wine-migrate--ensure-country region-entry-or-id))
         (region-enty (+brain-as-entry region-entry-or-id))
         (region-name (+brain-title region-entry-or-id))
         (region-file (wine-migrate--find-note region-name)))
    (unless region-file
      (let* ((org-roam-capture-templates-old org-roam-capture-templates)
             (org-roam-capture-templates `(("d" "default" plain
                                            #'org-roam-capture--get-point
                                            "%?"
                                            :file-name "wine/region/%<%Y%m%d%H%M%S>-${slug}"
                                            :head ,(concat
                                                    "#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
                                                    "- country :: "
                                                    (org-roam-format-link (plist-get result :country-file)
                                                                          (plist-get result :country-name))
                                                    "\n")
                                            :unnarrowed t
                                            :immediate-finish t))))
        (org-roam-find-file country-name nil nil t)
        (org-roam-db-build-cache)
        (setq org-roam-capture-templates org-roam-capture-templates-old)))
    (plist-put
     (plist-put
      result
      :region-file (wine-migrate--find-note region-name))
     :region-name region-name)))

(defun wine-migrate--ensure-country (region-entry-or-id)
  "Ensure that parent country of REGION-ENTRY-OR-ID exists.

Return (:country-file :country-name)."
  (let* ((country-entry (wine-migrate--find-country region-entry-or-id))
         (country-name (+brain-title country-entry))
         (country-file (wine-migrate--find-note country-name)))
    (unless country-file
      (let* ((org-roam-capture-templates-old org-roam-capture-templates)
             (org-roam-capture-templates `(("d" "default" plain
                                            #'org-roam-capture--get-point
                                            "%?"
                                            :file-name "wine/region/%<%Y%m%d%H%M%S>-${slug}"
                                            :head "#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
                                            :unnarrowed t
                                            :immediate-finish t))))
        (org-roam-find-file country-name nil nil t)
        (org-roam-db-build-cache)
        (setq org-roam-capture-templates org-roam-capture-templates-old)))
    (list :country-file (wine-migrate--find-note country-name)
          :country-name country-name)))

(defun wine/migrate-region ()
  "Migrate wine region to notes."
  (interactive)
  (unless (string-equal (+org-parent-id) (+brain-as-id wine-regions-parent))
    (user-error "Point must be at region"))
  (let* ((buffer (current-buffer))
         (file (buffer-file-name buffer))
         (children (+org-entry-get-list "BRAIN_CHILDREN" " "))
         (context (wine-migrate--ensure-appellation (org-id-get))))
    (switch-to-buffer buffer)
    (seq-do (lambda (child)
              (save-excursion
                (let ((p (cdr (org-id-find-id-in-file child file))))
                  (when (null p)
                    (user-error "Missing wine entry %s" child))
                  (goto-char p))
                (+org-entry-set "APPELLATION"
                                (org-roam-format-link (plist-get context :appellation-file)
                                                      (plist-get context :appellation-name)))
                (when (plist-get context :region-file)
                  (+org-entry-set "REGION"
                                  (org-roam-format-link (plist-get context :region-file)
                                                        (plist-get context :region-name))))
                (pretty-props/entry)))
            children)
    (org-cut-subtree)))

(provide '+org-wine)
;;; +org-wine.el ends here
