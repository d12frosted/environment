;;; +org-pretty-props.el --- Making properties pretty again -*- lexical-binding: t; -*-
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
(require '+org-brain)
(require '+org-link)
(require '+org-buffer-prop)

(declare-function org-map-entries "org")
(declare-function org-get-property-block"org")
(declare-function org-indent-line"org")

(defvar-local pretty-props-config '()
  "List of properties used for ordering.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+PROPERTIES_ORDER: PROP1 PROP2 PROP3 ...")

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

(defun pretty-props/buffer ()
  "Prettify properties of all entries in the buffer."
  (interactive)
  (org-map-entries #'pretty-props/entry
                   "prettify"
                   'file))

(defun pretty-props/entry ()
  "Prettify properties of entry at point.

- `pretty-props-format'
- `pretty-props-sort'"
  (interactive)
  (pretty-props-format)
  (pretty-props-sort))

(defun pretty-props-sort ()
  "Sort properties in entry at point."
  (let ((p0 (car (org-get-property-block)))
        (p1 (- (cdr (org-get-property-block)) 1))
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

(defun pretty-props-buffer-config ()
  "Get the `pretty-props-config' from current buffer."
  (+org-buffer-prop-get-list "PROPERTIES_ORDER"))

(provide '+org-pretty-props)
;;; +org-pretty-props.el ends here
