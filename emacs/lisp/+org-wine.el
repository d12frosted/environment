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
      (+org-prompt-property-brain "REGION" wine-regions-parent id 'parent)
      (+org-prompt-property-brain "STYLE" wine-styles-parent id 'parent)
      (save-buffer)
      (wine-refresh-entry)
      (save-buffer))))

(defun wine-refresh-entry ()
  "Refresh a wine entry at point."
  (org-edit-headline
   (wine-format-title wine-title-format))
  (pretty-props/entry))

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

(provide '+org-wine)
;;; +org-wine.el ends here
