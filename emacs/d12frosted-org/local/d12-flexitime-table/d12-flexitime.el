;;; d12-flexitime.el --- custom org-mode clock table formatter

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

(require 'org)

;;; Configuration variables
;;

(defvar d12-flexitime-weekday-duration 480
  "Duration of a work day in minutes.

This value is used by default but can be overridden for specific
flexitime table using :weekday minutes.")

;;; Type definitions
;;

(defclass d12-flexitime-day ()
  ((date
    :initarg :date
    :type list
    :documentation
    "Date in seconds")
   (dayType
    :initarg :dayType
    :type symbol
    :documentation
    "Type of the day. Available types are: weekday, weekend,
    holiday, vacation")
   (workDayDuration
    :initarg :workDayDuration
    :type number
    :documentation
    "Duration of work day in minutes.")
   (workedMinutes
    :initarg :workedMinutes
    :initform 0
    :type number
    :documentation
    "Amount of worked time in minutes.")
   (data
    :initarg :data
    :documentation
    "Hash table containing all data about this day.
Key is category name. Value category data.")))

(defmethod d12-flexitime-day-update-work-balance ((day d12-flexitime-day))
  "Update work balance.
Should be called after :data modification."
  (let ((total 0))
    (maphash (lambda (name category)
               (setq total (+ total
                              (oref category :workedMinutes))))
             (oref day :data))
    (oset day :workedMinutes total)))

(defmethod d12-flexitime-day-get-work-balance ((day d12-flexitime-day))
  "Get work balance in minutes.
Positive value means overtime. Negative means that you have to
work more!"
  (- (oref day :workedMinutes)
     (oref day :workDayDuration)))

(defmethod d12-flexitime-day-get-category ((day d12-flexitime-day) name)
  "Get category data by NAME."
  (let ((category (gethash name (oref day :data))))
    (unless category
      (setq category
            (puthash name
                     (d12-flexitime-category
                      :name name
                      :data (make-hash-table :test 'equal))
                     (oref day :data))))
    category))

(defclass d12-flexitime-category ()
  ((name
    :initarg :name
    :type string
    :documentation
    "Category name.")
   (workedMinutes
    :initarg :workedMinutes
    :initform 0
    :type number
    :documentation
    "Amount of worked time in minutes.")
   (data
    :initarg :data
    :documentation
    "Hash table containing all data about this category.
Key is headline name. Value is headline data.")))

(defmethod d12-flexitime-category-add-headline ((category d12-flexitime-category) headline)
  "Add HEADLINE to category data."
  (let ((name (oref headline :name)))
    (puthash name headline (oref category :data)))
  (when (eql 1 (oref headline :level))
    (oset category :workedMinutes
          (+ (oref category :workedMinutes)
             (oref headline :time)))))

(defclass d12-flexitime-headline ()
  ((name
    :initarg :name
    :type string
    :documentation
    "Headline name.")
   (time
    :initarg :time
    :type number
    :documentation
    "Clocked time in minutes.")
   (level
    :initarg :level
    :type number
    :documentation
    "Heading level.")))

;;;###autoload
(defun org-dblock-write:flexitime (params)
  "Write the standard flexitime table."
  (plist-put params :formatter 'd12-flexitime--day-formatter)
  (plist-put params :properties '("CATEGORY" "ARCHIVE_CATEGORY"))
  (let ((days (d12-flexitime--generate-days params)))
    (plist-put params :days days)
    (insert "| Date | Category | Headline | Time | Overtime |\n")
    (insert "|------+----------+----------+------+----------|\n")
    (dolist (day days)
      (plist-put params :day day)
      (plist-put params :block nil)
      (plist-put params
                 :tstart
                 (format-time-string (car org-time-stamp-formats)
                                     (oref day :date)))
      (plist-put params :tend
                 (format-time-string (car org-time-stamp-formats)
                                     (seconds-to-time (+ (time-to-seconds (oref day :date))
                                                         86400))))
      (org-dblock-write:clocktable params)))
  (insert "| *Total* | | | | |\n")
  ;; (maphash
  ;;  (lambda (cat time)
  ;;    (insert (format "| | *%s* | | %s | |\n" cat (d12-flexitime--format-minutes time))))
  ;;  (plist-get params :data))
  ;; (insert "|------+----------+----------+------+----------|\n")
  (org-table-align)
  (delete-forward-char -1))

;;; Gore
;; It's dangerous to walk further without a good portion of painkillers

(defun d12-flexitime--day-formatter (ipos tables params)
  "Write out a flexitime day clock table at position IPOS in the current buffer.
  TABLES is a list of tables with clocking data as produced by
  `org-clock-get-table-data'. PARAMS is the parameter property
  list obtained from the dynamic block definition."
  (let ((day (plist-get params :day)))
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
            (let ((category (d12-flexitime-day-get-category day cat)))
              (d12-flexitime-category-add-headline
               category
               (d12-flexitime-headline
                :name headline
                :time time
                :level level)))))))

    (d12-flexitime-day-update-work-balance day)
    ;; now print data
    (unless (hash-table-empty-p (oref day :data))
      (insert (format "| %s | | | *%s* | *%s* |\n"
                      (plist-get params :tstart)
                      (d12-flexitime--format-minutes (oref day :workedMinutes))
                      (d12-flexitime--format-minutes (d12-flexitime-day-get-work-balance day))))
      (maphash
       (lambda (category-name category)
         (insert "| | | | | |\n")
         (insert (format "| | *%s* | | *%s* | |\n"
                         category-name
                         (d12-flexitime--format-minutes (oref category :workedMinutes))))
         (maphash
          (lambda (hl-name hl)
            (if (> (oref hl :level) 1)
                (insert (format "| | | \\_ %s | %s | |\n" hl-name (d12-flexitime--format-minutes (oref hl :time))))
              (insert (format "| | | %s | *%s* | |\n" hl-name (d12-flexitime--format-minutes (oref hl :time))))))
          (oref category :data)))
       (oref day :data))
      (insert "|------+----------+----------+------+----------|\n"))))

(defun d12-flexitime--generate-days (params)
  (when (not (eq 'thismonth (plist-get params :block)))
    (error "Only 'thismonth is supported as a :block"))
  (let* ((today (calendar-current-date))
         (month (calendar-extract-month today))
         (year (calendar-extract-year today))
         (start (org-time-string-to-seconds (format "%4d-%02d-01" year month)))
         (end (org-time-string-to-seconds (format "%4d-%02d-01" year (+ month 1))))
         (res '()))
    (while (< start end)
      (add-to-list 'res
                   (d12-flexitime-day
                    :date (seconds-to-time start)
                    :dayType 'weekday
                    :workDayDuration d12-flexitime-weekday-duration
                    :data (make-hash-table :test 'equal))
                   t)
      (setq start (+ start 86400)))
    res))

;;; Helpers
;;

(defun d12-flexitime--format-minutes (minutes)
  "Properly format MINUTES for clock table."
  (format "%s%s"
          (if (< minutes 0) "-" "")
          (org-minutes-to-clocksum-string (abs minutes))))

;;; d12-flexitime.el ends here
