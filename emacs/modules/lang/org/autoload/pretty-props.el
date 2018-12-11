;;; lang/org/autoload/pretty-props.el -*- lexical-binding: t; -*-
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
(define-minor-mode pretty-props-mode
  "Minor mode for keeping Org properties in a pretty state."
  :lighter " pretty-props"
  (setq-local pretty-props-config (pretty-props-buffer-config)))

;;;###autoload
(defun pretty-props-mode-maybe-enable ()
  "Conditionally enable `pretty-props-mode' in the `org-mode' buffer.

Enables the `pretty-props-mode' iff the buffer has
'pretty-props-mode:t' option set in the options section.

  #+OPTIONS: pretty-props-mode:t"
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*pretty-props-mode:t" (point-max) t)
        (pretty-props-mode)))))

(defvar-local pretty-props-config '()
  "List of properties used for ordering.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+PROPERTIES_ORDER: PROP1 PROP2 PROP3 ...")

(defun pretty-props/buffer ()
  "Prettify properties of all entries in the buffer."
  (interactive)
  (org-map-entries #'pretty-props/entry
                   "prettify"
                   'file))

(defun pretty-props/entry ()
  "Prettify properties of entry at point.

- `pretty-props-format'
- `pretty-props-sort'
- `pretty-props-update-links'"
  (interactive)
  (pretty-props-format)
  (pretty-props-sort)
  (pretty-props-update-links))

(defun pretty-props-sort ()
  "Sort properties in entry at point."
  (let ((p0 (car (org-get-property-block)))
        (p1 (- (cdr (org-get-property-block)) 1))
        (props (org-entry-properties))
        (maxv (seq-length pretty-props-config))
        (pregx "^:\\([a-zA-Z_\\-]+\\):.*$"))
    (save-excursion
      (save-restriction
        (narrow-to-region p0 p1)
        (goto-char (point-min))
        (let ;; To make `end-of-line' and etc. to ignore fields.
            ((inhibit-field-text-motion t))
          (sort-subr
           nil 'forward-line 'end-of-line nil nil
           (lambda (l1 l2)
             (< (or
                 (seq-position pretty-props-config
                               (+string-match-1 pregx l1))
                 maxv)
                (or
                 (seq-position pretty-props-config
                               (+string-match-1 pregx l2))
                 maxv)))))))))

(defun pretty-props-format ()
  "Format properties in entry at point."
  (let ((p0 (car (org-get-property-block)))
        (p1 (cdr (org-get-property-block))))
    (save-excursion
      (goto-char p0)
      (while (< (point) p1)
        (org-indent-line)
        (forward-line 1)
        (beginning-of-line)))))

(defun pretty-props-update-links ()
  "Update titles of the links in properties."
  (let ((p0 (car (org-get-property-block)))
        (p1 (cdr (org-get-property-block))))
    (save-excursion
      (save-restriction
        (narrow-to-region p0 p1)
        (goto-char (point-min))
        (while (search-forward-regexp +org-id-link-regexp nil t)
          (replace-match (+brain-title (match-string 2))
                         nil nil nil 3))))))

(defun pretty-props-buffer-config ()
  "Get the `pretty-props-config' from current buffer."
  (+org-get-buffer-settings "PROPERTIES_ORDER"))
