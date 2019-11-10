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
(require '+org-settings)
(require '+inventory)

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

(def-org-buffer-brain-entry-setting
  wine-parent
  'wine-mode-hook
  "WINE_PARENT"
  "ID of wine parent entry.")

(def-org-buffer-setting
  wine-title-format
  nil
  'wine-mode-hook
  "WINE_TITLE_FORMAT"
  "Format of the wine entry title.")

(def-org-buffer-setting
  wine-rating-title-format
  nil
  'wine-mode-hook
  "WINE_RATING_TITLE_FORMAT"
  "Format of the wine entry title.")

(def-org-buffer-setting
  wine-inventory-file
  nil
  'wine-mode-hook
  "INVENTORY_FILE"
  "File name of the inventory.")

(def-org-buffer-brain-entry-setting
  wine-wineries-parent
  'wine-mode-hook
  "WINERIES_PARENT"
  "ID of wineries parent entry.")

(def-org-buffer-brain-entry-setting
  wine-regions-parent
  'wine-mode-hook
  "REGIONS_PARENT"
  "ID of regions parent entry.")

(def-org-buffer-brain-entry-setting
  wine-styles-parent
  'wine-mode-hook
  "STYLES_PARENT"
  "ID of styles parent entry.")

(def-org-buffer-brain-entry-setting
  wine-grapes-parent
  'wine-mode-hook
  "GRAPES_PARENT"
  "ID of grapes parent entry.")

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
  (+brain-choose-entry-by-parent
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
      (+org-prompt-number-property "YEAR")
      (+org-prompt-brain-property "REGION" wine-regions-parent id 'parent)
      (+org-prompt-property-repeating
       #'+org-prompt-brain-property-fn
       "GRAPES"
       wine-grapes-parent)
      ;; (+org-prompt-brain-property "STYLE" wine-styles-parent id 'parent)
      (save-buffer)
      (wine-refresh-entry)
      (save-buffer))))

(defun wine-refresh-entry ()
  "Refresh a wine entry at point."
  (let ((id (org-id-get-create)))
    (+org-entry-set-number "TOTAL_IN"
                           (inventory-total-in wine-inventory-file id))
    (+org-entry-set-number "TOTAL_OUT"
                           (inventory-total-out wine-inventory-file id)))
  (+org-entry-set-number "AVAILABLE"
                         (round (- (+org-entry-get-number "TOTAL_IN")
                                   (+org-entry-get-number "TOTAL_OUT"))))
  (+org-entry-set-average-number "RATE" "TOTAL" "RATING" #'wine-refresh-rating)
  (org-edit-headline
   (wine-format-title wine-title-format))
  (pretty-props/entry))

;;
;; Ratings

(defun wine-refresh-rating (&optional propagate)
  "Refresh rating entry at point.

When PROPAGATE is non-nil, refresh is propagated upper to the
wine entry."
  (if propagate
    (save-excursion
      (org-up-heading-safe)
      (wine-refresh-entry))
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
;; Refresh

(defun wine/refresh ()
  "Refresh entry at point.

Supports the following entries:

1. Wine entry
2. Rating entry"
  (interactive)
  (cond
   ((string-equal (+org-parent-id) (+brain-as-id wine-parent))
    (wine-refresh-entry))
   ((+org-entry-tag-p "RATING")
    (wine-refresh-rating t))
   (t
    (message "Unsupported entry"))))

(defun wine/refresh-buffer ()
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
        (wine--refresh-mapping)))

(defun wine--refresh-mapping ()
  "Return alist of parent id and refresh function."
  (list (cons (+brain-as-id wine-parent) #'wine-refresh-entry)))

(provide '+org-wine)
;;; +org-wine.el ends here
