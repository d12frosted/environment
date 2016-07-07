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

(defvar d12-flexitime-work-day-duration 540
  "Duration of a work day in minutes.")

;;;###autoload
(defun org-dblock-write:flexitime (params)
  "Write the standard flexitime table."
  (plist-put params :formatter 'd12-flexitime--day-formatter)
  (plist-put params :properties '("CATEGORY" "ARCHIVE_CATEGORY"))
  (let ((dates (d12-flexitime--generate-dates-list params)))
    (plist-put params :dates dates)
    (plist-put params :data (make-hash-table :test 'equal))
    (insert "| Date | Category | Headline | Time | Overtime |\n")
    (insert "|------+----------+----------+------+----------|\n")
    (dolist (date dates)
      (plist-put params :block nil)
      (plist-put params
                 :tstart
                 (format-time-string (car org-time-stamp-formats)
                                     (seconds-to-time date)))
      (plist-put params
                 :tend
                 (format-time-string (car org-time-stamp-formats)
                                     (seconds-to-time (+ date 86400))))
      (org-dblock-write:clocktable params)
      (insert "|------+----------+----------+------+----------|\n")))
  (insert "| *Total* | | | | |\n")
  (maphash
   (lambda (cat time)
     (insert (format "| | *%s* | | %s | |\n" cat (d12-flexitime--format-minutes time))))
   (plist-get params :data))
  (insert "|------+----------+----------+------+----------|\n")
  (org-table-align)
  (delete-forward-char -1))

;;; Gore
;; It's dangerous to walk further without a good portion of painkillers

(defun d12-flexitime--day-formatter (ipos tables params)
  "Write out a flexitime day clock table at position IPOS in the current buffer.
  TABLES is a list of tables with clocking data as produced by
  `org-clock-get-table-data'. PARAMS is the parameter property
  list obtained from the dynamic block definition."
  (let ((data (plist-get params :data)) ; global data for total section
        ;; data for current day, where key is category name and value is another
        ;; hash table with headline as key and plist as a value
        (dayData (make-hash-table :test 'equal))
        ;; total data for category for this day
        (catTimeMap (make-hash-table :test 'equal))
        (level 1)
        (dayTotal 0))

    ;; gather dayData
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
            (let ((catMap (gethash cat dayData)))
              (unless catMap
                (setq catMap (puthash cat (make-hash-table :test 'equal) dayData)))
              (let ((pl '()))
                (setq pl (plist-put pl :level level))
                (setq pl (plist-put pl :time time))
                (puthash headline pl catMap)))))))

    ;; calculate total for this day and write it to data and catTimeMap
    (maphash
     (lambda (cat catMap)
       (maphash
        (lambda (headline pl)
          (when (eq 1 (plist-get pl :level))
            (mapc (lambda (table)
                    (let ((oldCatTime (gethash cat table)))
                      (unless oldCatTime
                        (setq oldCatTime (puthash cat 0 table)))
                      (puthash cat (+ (plist-get pl :time) oldCatTime) table)))
                  `(,data ,catTimeMap))))
        catMap))
     dayData)
    (maphash (lambda (cat catTotal)
               (setq dayTotal (+ dayTotal catTotal)))
             catTimeMap)

    ;; now print data
    (insert (format "| %s | | | *%s* | *%s* |\n"
                    (plist-get params :tstart)
                    (d12-flexitime--format-minutes dayTotal)
                    (d12-flexitime--format-minutes (- dayTotal d12-flexitime-work-day-duration))))
    (maphash
     (lambda (cat catMap)
       (insert "| | | | | |\n")
       (insert (format "| | *%s* | | *%s* | |\n" cat (d12-flexitime--format-minutes (gethash cat catTimeMap))))
       (maphash
        (lambda (headline pl)
          (if (> (plist-get pl :level) level)
              (insert (format "| | | \\_ %s | %s | |\n" headline (d12-flexitime--format-minutes (plist-get pl :time))))
            (insert (format "| | | %s | *%s* | |\n" headline (d12-flexitime--format-minutes (plist-get pl :time))))))
        catMap))
     dayData)))

(defun d12-flexitime--generate-dates-list (params)
  (when (not (eq 'thismonth (plist-get params :block)))
    (error "Only 'thismonth is supported as a :block"))
  (let* ((today (calendar-current-date))
         (month (calendar-extract-month today))
         (year (calendar-extract-year today))
         (start (org-time-string-to-seconds (format "%4d-%02d-01" year month)))
         (end (org-time-string-to-seconds (format "%4d-%02d-01" year (+ month 1))))
         (res '()))
    (while (< start end)
      (add-to-list 'res start t)
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
