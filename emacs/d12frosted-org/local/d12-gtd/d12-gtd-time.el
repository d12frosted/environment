;;; d12-gtd-time.el --- time helpers for personal GTD implementation

;; Copyright (c) 2017 Boris Buliga

;; Author: Boris Buliga <d12frosted@gmail.com>
;; Maintainer: Boris Buliga <d12frosted@gmail.com>
;; Created: 09 Jan 2017

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(defun d12-gtd-time-parse (str)
  "Return internal time from STR.

This function also properly handles dates like 2016-12-19 by
setting hours, minutes and seconds to 0."
  (apply #'encode-time
         (apply
          (lambda (sec min hour &rest args)
            (append `(,(if sec sec 0)
                      ,(if min min 0)
                      ,(if hour hour 0))
                    args))
          (parse-time-string str))))

(defun d12-gtd-date-eq (time1 time2)
  "Compare two time values discarding time components."
  (= (time-to-days time1)
     (time-to-days time2)))

(defun d12-gtd-date-gte (time1 time2)
  "Compare two time values discarding time components."
  (>= (time-to-days time1)
      (time-to-days time2)))

(defun d12-gtd-date-lte (time1 time2)
  "Compare two time values discarding time components."
  (<= (time-to-days time1)
     (time-to-days time2)))

(provide 'd12-gtd-time)

;;; d12-gtd-time.el ends here
