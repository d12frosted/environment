;;; config.el --- d12frosted-org Layer config File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Variables

(defvar d12frosted/org-home-path "~/org/"
  "Directory where org files are stored.

   All org files in this directory and all subdirectories will be used as agenda
   files for org agenda. If you want to ignore some files, checkout
   'd12frosted/org-agenda-ignore-dirs variable. Currently you can ignore files
   only by putting them into ignored directory.

   Warning! Don't forget to add trailing slash at the end.")

(defvar d12frosted/org-agenda-ignore-dirs
  '("temporary/"
    "tmp/")
  "List of directories to be ignored for agenda. Every path should be relative
   to d12frosted/org-home-path and must contain trailing slash.")

(defvar d12frosted/org-time-format
  "%H:%M:%S"
  "Format for 'd12frosted/org-insert-time function.")

(defvar d12frosted/org-date-format
  "%d %B %Y, %A"
  "Format for 'd12frosted/org-insert-date function.")

(defvar d12frosted/org-default-title
  "Yet another org file"
  "Default title for org files. Is used by 'd12frosted/org-guess-title
   when it failt to get the ttitle from buffer name.")

(defvar d12frosted/org-author-name
  "John Doe"
  "Author name (used in 'd12frosted/org-new-file-template function.)")

(defvar d12frosted/org-author-email
  "example@domain.com"
  "Author email (used in 'd12frosted/org-new-file-template function.)")
