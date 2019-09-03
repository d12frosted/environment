;;; lang/org/autoload/cigars.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 03 Sep 2019
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
(define-minor-mode cigars-mode
  "Minor mode for all the cigars utilities."
  :lighter " 🚬"
  (places-mode)
  (pretty-props-mode)

  (setq-local cigars-parent-id
              (+org-get-buffer-setting "CIGARS_PARENT"))
  (setq-local cigars--parent
              (+brain-as-entry cigars-parent-id))

  (setq-local cigar-title-format
              (+org-get-buffer-setting "CIGAR_TITLE_FORMAT"))
  (setq-local cigar-rating-title-format
              (+org-get-buffer-setting "CIGAR_RATING_TITLE_FORMAT")))

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
;; Refresh

(defun cigar/refresh ()
  "Refresh entry at point.

Supports the following entries:

1. Cigar entry
2. Rating entry"
  (interactive)
  (cond
   ((string-equal (+org-parent-id) cigars-parent-id)
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
  (list (cons cigars-parent-id #'cigar-refresh-entry)))

(defun cigar-refresh-entry ()
  "Refresh a cigar entry at point."
  (let ((id (org-id-get-create)))
    (+org-entry-set-number "TOTAL_IN" (cigar-inv--total-in id))
    (+org-entry-set-number "TOTAL_OUT" (cigar-inv--total-out id)))
  (+org-entry-set-number "AVAILABLE"
                         (round (- (+org-entry-get-number "TOTAL_IN")
                                   (+org-entry-get-number "TOTAL_OUT"))))
  (let ((rates (org-map-entries
                (lambda ()
                  (cigar-refresh-rating nil)
                  (+org-entry-get-number "TOTAL"))
                "+RATING"
                'tree)))
    (+org-entry-set-number "RATE"
                           (if (null rates)
                               0
                             (/ (apply #'+ rates)
                                (float (length rates))))))
  (org-edit-headline
   (cigar-format-title cigar-title-format))
  (pretty-props/entry))

(defun cigar-refresh-rating (propagate)
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

(defvar-local cigar-title-format nil
  "Format of the cigar entry title.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+CIGAR_TITLE_FORMAT: {LOCATION} {NAME} | {YEAR} | {TAG}")

(defvar-local cigar-rating-title-format nil
  "Format of the rating entry title.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+CIGAR_RATING_TITLE_FORMAT: {LOCATION} {NAME} | {YEAR} | {TAG}")

(defun cigar-format-title (format)
  (let ((result format)
        (properties (org-entry-properties))
        (prop-regexp "{\\([a-zA-Z0-9\\-_]+\\):?\\([%\\.0-9A-Za-z]*\\)")
        (prop "")
        (val "")
        (val-format ""))
    (while (string-match prop-regexp result)
      (setq prop (match-string 1 result))
      (setq val-maybe-format (match-string 2 result))
      (setq val-format (if (string-empty-p val-maybe-format) "%s" val-maybe-format))
      (setq val-format-regexp
            (unless (string-empty-p val-maybe-format)
                (concat ":" val-maybe-format)))
      (setq val-original (cdr (assoc-string prop properties)))
      (setq val (replace-regexp-in-string
                 "\\[\\[.+\\]\\[\\(.+\\)\\]\\]"
                 "\\1"
                 val-original))
      (setq val-conv
            (if (or (string-suffix-p "f" val-format)
                    (string-suffix-p "d" val-format))
                (string-to-number val)
              val))
      (setq result (replace-regexp-in-string
                    (concat "{" prop val-format-regexp "}")
                    (or (format val-format val-conv) "?")
                    result
                    t)))
    result))

;;
;; Cigars

(defvar cigars-parent-id ""
  "ID of Tea Groups parent entry.

Can be set in the org-mode buffer by adding following line in the
top of the file:

  #+CIGARS_PARENT: ID")

(defvar-local cigars--parent nil)

(defun cigar/acquire (&optional source id amount date)
  "Acquire AMOUNT of ID because from SOURCE at DATE."
  (interactive)
  (let ((id (or id (org-id-get-create)))
        (source (or source (read-string "Source: " "cigarworld.de")))
        (amount (or amount (read-number "Amount: ")))
        (date (or date (org-read-date nil t))))
    (cigar-inv--add id amount source date)
    (cigar-refresh-entry)))

(defun cigar/consume (&optional action id amount date)
  "Consume AMOUNT of ID because of ACTION at DATE."
  (interactive)
  (let ((id (or id (org-id-get-create)))
        (action (or action (read-string "Action: " "consume")))
        (amount (or amount (read-number
                            "Amount: "
                            (+org-entry-get-number "DEFAULT_AMOUNT"))))
        (date (or date (org-read-date nil t))))
    (cigar-inv--sub id amount action date)
    (when (and (string-equal action "consume")
               (y-or-n-p "Rate?"))
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
      (org-set-tags-to ":RATING:")
      (org-set-property "DATE" (format-time-string "%Y-%m-%d" date))
      (+org-prompt-property "TOTAL")
      (save-buffer)
      (cha-refresh-tea-rating t))))

;;
;; Inventory

(defun cigar-inv--balance (id &optional query)
  "Get balance of ID using QUERY."
  (let* ((cmd (format "hledger -f cigars.journal b %s '%s'" id query))
         (res (shell-command-to-string cmd))
         (lines (split-string res "\n")))
    (string-to-number (car (seq-drop-while #'string-empty-p (reverse lines))))))

(defun cigar-inv--total-in (id)
  "Get total income for ID."
  (cigar-inv--balance id "amt:>0"))

(defun cigar-inv--total-out (id)
  "Get total outcome for ID."
  (abs (cigar-inv--balance id "amt:<0")))

(defun cigar-inv--add (id amount source &optional date)
  "Add AMOUNT of ID to inventory from SOURCE.

When DATE is omitted, `current-time' is used."
  (shell-command-to-string
   (format
    "echo '\n%s\n    cigar:%s  %s\n    source:%s' >> cigars.journal"
    (format-time-string "%Y/%m/%d" date)
    id
    amount
    source)))

(defun cigar-inv--sub (id amount action &optional date)
  "Subtract amount of ID from inventory as result of ACTION.

When DATE is omitted, `current-time' is used."
  (shell-command-to-string
   (format
    "echo '\n%s\n    activity:%s  %s\n    cigar:%s' >> cigars.journal"
    (format-time-string "%Y/%m/%d" date)
    action
    amount
    id)))
