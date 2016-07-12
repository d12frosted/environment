;;; flexitime.el --- custom org-mode clock table formatter

;; Copyright (c) 2016 Boris Buliga

;; Author: Boris Buliga <d12frosted@gmail.com>
;; Maintainer: Boris Buliga <d12frosted@gmail.com>
;; Created: 05 Jul 2016

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ((org))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; | Date    | Category | Headline |    Time | Overtime |
;; |---------+----------+----------+---------+----------|
;; | *D1*    |          |          |  *9:00* | *1:00*   |
;; |         | *C1*     |          |  *8:00* |          |
;; |         |          | T1       |    2:00 |          |
;; |         |          | T2       |    6:00 |          |
;; |         | *C2*     |          |  *1:00* |          |
;; |         |          | T1       |    1:00 |          |
;; |---------+----------+----------+---------+----------|
;; | *D2*    |          |          |  *6:00* | *-2:00*  |
;; |         | *C1*     |          |  *6:00* |          |
;; |         |          | T1       |    5:00 |          |
;; |         |          | T2       |    1:00 |          |
;; |---------+----------+----------+---------+----------|
;; | *Total* |          |          | *15:00* | *-1:00*  |
;; |         | *C1*     |          |   14:00 |          |
;; |         | *C2*     |          |    1:00 |          |

;;; Code:
;;

(require 'org-clock)

;;; * Configuration variables
;;

(defvar flexitime-weekday-duration 480
  "Duration of a work day in minutes.

This value is used by default but can be overridden for specific
flexitime table using :weekday minutes.")

(defvar flexitime-skip-empty-days t
  "When non-nil skip empty days in flexitime table.

This value is used by default but can be overridden for specific
flexitime table using :skip-empty-days val.")

(defvar flexitime-holidays-category-name "Holidays")
(defvar flexitime-vacation-category-name "Vacation")
(defvar flexitime-weekends '("Sat" "Sun"))

;;; * Type definitions
;;

;;; ** `flexitime-day' definition
;;

(cl-defstruct (flexitime-day (:constructor flexitime-day--create))
  (date nil :read-only t)
  (type nil :read-only t)
  (duration nil :read-only t)
  (time 0)
  (data (make-hash-table :test 'equal)))

(defun flexitime-day-create (date)
  "Create `flexitime-day' for a given DATE."
  (let ((type (flexitime--figure-day-type date)))
    (flexitime-day--create
     :date date
     :type type
     :duration (if (eq type 'weekday)
                   flexitime-weekday-duration
                 0))))

(defmethod flexitime-day-update-work-balance ((day flexitime-day))
  "Update work balance.
Should be called after :data modification."
  (maphash (lambda (name category)
             (cl-incf (flexitime-day-time day)
                      (flexitime-category-time category)))
           (flexitime-day-data day))
  (flexitime-day-time day))

(defmethod flexitime-day-get-work-balance ((day flexitime-day))
  "Get work balance in minutes.
Positive value means overtime. Negative means that you have to
work more!"
  (- (flexitime-day-time day)
     (flexitime-day-duration day)))

(defmethod flexitime-day-get-category ((day flexitime-day) name)
  "Get category data by NAME."
  (let ((category (gethash name (flexitime-day-data day))))
    (unless category
      (setq category (puthash
                      name
                      (flexitime-category-create name)
                      (flexitime-day-data day))))
    category))

;;; ** `flexitime-category' definition
;;

(cl-defstruct (flexitime-category (:constructor flexitime-category--create))
  (name nil :read-only t)
  (time 0)
  (data (make-hash-table :test 'equal)))

(defun flexitime-category-create (name)
  "Create `flexitime-categry' for a given NAME."
  (flexitime-category--create :name name))

(defmethod flexitime-category-add-headline ((category flexitime-category) headline)
  "Add HEADLINE to category data."
  (let ((name (flexitime-headline-name headline)))
    (puthash name headline (flexitime-category-data category)))
  (when (eql 1 (flexitime-headline-level headline))
    (cl-incf (flexitime-category-time category)
             (flexitime-headline-time headline))))

;;; ** `flexitime-headline' definition
;;

(cl-defstruct (flexitime-headline (:constructor flexitime-headline-create))
  (name nil :read-only t)
  (time 0)
  (level nil :read-only t))

;;; * Flexitime table writer
;;

;;;###autoload
(defun org-dblock-write:flexitime (params)
  "Write the standard flexitime table."
  (plist-put params :formatter 'flexitime--day-formatter)
  (plist-put params :properties '("CATEGORY" "ARCHIVE_CATEGORY"))
  (let ((print-category (flexitime--plist-get params :print-category t))
        (days (flexitime--generate-days params)))
    (plist-put params :days days)
    (flexitime--format-row "Date" "Category" "Headline" "Time" "Overtime")
    (flexitime--format-row 'sep 'sep 'sep 'sep 'sep)
    (dolist (day days)
      (plist-put params :day day)
      (plist-put params :block nil)
      (plist-put params
                 :tstart
                 (format-time-string (car org-time-stamp-formats)
                                     (flexitime-day-date day)))
      (plist-put params :tend
                 (format-time-string (car org-time-stamp-formats)
                                     (seconds-to-time (+ (time-to-seconds (flexitime-day-date day))
                                                         86400))))
      (org-dblock-write:clocktable params))

    ;; calculate and print total
    (let ((totalByKey (make-hash-table :test 'equal))
          (totalWorked 0)
          (totalBalance 0))
      (mapc
       (lambda (day)
         (maphash
          (lambda (cat-name cat)
            (when print-category
              (puthash cat-name
                       (+ (gethash cat-name totalByKey 0)
                          (flexitime-category-time cat))
                       totalByKey))
            (unless print-category
              (maphash
               (lambda (hl-name hl)
                 (when (eql (flexitime-headline-level hl) 1)
                   (puthash hl-name
                          (+ (gethash hl-name totalByKey 0)
                             (flexitime-headline-time hl))
                          totalByKey)))
               (flexitime-category-data cat))))
          (flexitime-day-data day))
         (setq totalWorked (+ (flexitime-day-time day) totalWorked))
         (setq totalBalance (+ (flexitime-day-get-work-balance day) totalBalance)))
       days)
      (flexitime--format-row
       '(bold "Total") "" ""
       `(bold ,totalWorked)
       `(bold ,totalBalance))
      (maphash
       (lambda (key time)
         (flexitime--format-row
          "" key (if print-category "" key) time ""))
       totalByKey)
      (flexitime--format-row 'sep 'sep 'sep 'sep 'sep))
    (org-table-align)
    (delete-forward-char -1)))

;;; * Day formatter
;;

(defun flexitime--day-formatter (ipos tables params)
  "Write out a flexitime day clock table at position IPOS in the current buffer.
  TABLES is a list of tables with clocking data as produced by
  `org-clock-get-table-data'. PARAMS is the parameter property
  list obtained from the dynamic block definition."
  (let ((print-category (flexitime--plist-get params :print-category t))
        (day (plist-get params :day)))
    ;; gather data for day
    (while (setq tbl (pop tables))
      ;; now tbl is the table resulting from one file.
      (setq file-time (nth 1 tbl))
      (when (or (and file-time (> file-time 0))
                (not (plist-get params :fileskip0)))

        ;; Get the list of node entries and iterate over it
        (setq entries (nth 2 tbl))
        (while (setq entry (pop entries))
          (let ((level (nth 0 entry))
                (headline (nth 1 entry))
                (timestamp (nth 2 entry))
                (time (nth 3 entry))
                (cat (or (cdr (assoc "ARCHIVE_CATEGORY" (nth 4 entry)))
                         (cdr (assoc "CATEGORY" (nth 4 entry))))))
            (let ((category (flexitime-day-get-category day cat)))
              (flexitime-category-add-headline
               category
               (flexitime-headline-create
                :name headline
                :time time
                :level level)))))))

    (flexitime-day-update-work-balance day)
    ;; now print data
    (unless (and flexitime-skip-empty-days
                 (hash-table-empty-p (flexitime-day-data day)))
      (flexitime--format-row
       (plist-get params :tstart) "" ""
       (flexitime-day-time day)
       (flexitime-day-get-work-balance day))
      (maphash
       (lambda (category-name category)
         (flexitime--format-row "" "" "" "" "")
         (when print-category
           (flexitime--format-row
            "" `(bold ,category-name)
            "" `(bold ,(flexitime-category-time category)) ""))
         (maphash
          (lambda (hl-name hl)
            (flexitime--format-row
             "" ""
             `(level ,(flexitime-headline-level hl) ,hl-name)
             (if (or print-category
                     (> (flexitime-headline-level hl) 1))
                 (flexitime-headline-time hl)
               `(bold ,(flexitime-headline-time hl)))
             ""))
          (flexitime-category-data category)))
       (flexitime-day-data day))
      (flexitime--format-row 'sep 'sep 'sep 'sep 'sep))))

;;; * Day generation
;;

(defun flexitime--generate-days (params)
  (let ((block (plist-get params :block))
        (ws (plist-get params :wstart))
        (ms (plist-get params :mstart))
        (te (plist-get params :tend))
        (cc) (ts) (te) (range-text) (res))
    (setq cc (org-clock-special-range block nil t ws ms)
          ts (org-time-string-to-seconds (car cc))
          te (org-time-string-to-seconds (nth 1 cc))
          range-text (nth 2 cc))
    (while (< ts te)
      (add-to-list 'res (flexitime-day-create (seconds-to-time ts)) t)
      (cl-incf ts 86400))
    res))

(defun flexitime--figure-day-type (time)
  (when (format-time-string "%a" time))
  (let ((type 'weekday))
    (if (member (format-time-string "%a" time)
                flexitime-weekends)
        (setq type 'weekend)
      (dolist (file (org-agenda-files nil 'ifmode))
        (dolist (entry (org-agenda-get-day-entries
                        file (flexitime--time-to-date time)))
          (let ((category (with-temp-buffer
                            (insert entry)
                            (org-get-category (point-min)))))
            (when (string= flexitime-holidays-category-name category)
              (setq type 'holiday))
            (when (string= flexitime-vacation-category-name category)
              (setq type 'vacation))))))
    type))

;;; * Helpers
;;

(defun flexitime--format-cell (data)
  "Format flexitime table cell."
  (pcase data
    (`sep "-")
    (`(bold ,value)
      (format "*%s*" (flexitime--format-cell value)))
    (`(level ,lvl ,value)
      (format
       "%s%s"
       (if (> lvl 1) "\\_ " "")
       (flexitime--format-cell value)))
    ((pred stringp) data)
    ((pred numberp) (flexitime--format-minutes data))
    (_ (format "%s" data))))

(defun flexitime--format-row (date cat hl time over)
  "Format flexitime table row using `flexitime--format-cell'."
  (insert "|")
  (insert (flexitime--format-cell date))
  ;; dynamic binding makes it possible
  (when print-category
    (insert "|")
    (insert (flexitime--format-cell cat)))
  (insert "|")
  (insert (flexitime--format-cell hl))
  (insert "|")
  (insert (flexitime--format-cell time))
  (insert "|")
  (insert (flexitime--format-cell over))
  (insert "|\n"))

(defun flexitime--format-minutes (minutes)
  "Properly format MINUTES for clock table."
  (format "%s%s"
          (if (< minutes 0) "-" "")
          (org-minutes-to-clocksum-string (abs minutes))))

(defun flexitime--time-to-date (&optional time)
  "Convert TIME to DATE.

TIME as returned by `current-time'.
DATE as returned by `calendar-current-date'.

When TIME is omitted, `current-time' is used instead."
  (let ((lt (decode-time (or time
                             (current-time)))))
    `(,(nth 4 lt) ,(nth 3 lt) ,(nth 5 lt))))

(defun flexitime--plist-get (plist prop &optional default)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or DEFAULT if PROP is not one of the
properties on the list.  This function never signals an error."
  (if (plist-member plist prop)
      (plist-get plist prop)
    default))

(provide 'flexitime)

;;; flexitime.el ends here
