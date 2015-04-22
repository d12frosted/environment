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

This directory is recursively searched for org files.

Don't forget to add trailing slash at the end.")

(defvar d12frosted/org-agenda-ignore-dirs
  '("temporary/"
    "tmp/")
  "List of directories to be ignored for agenda. Every path should be relative to d12frosted/org-home-path and should contain trailing slash.")

(defvar d12frosted/org-time-format
  "%H:%M:%S")

(defvar d12frosted/org-date-format
  "%d %B %Y, %A")

(defvar d12frosted/org-default-title
  "Yet another org file")

(defvar d12frosted/org-author-name
  "John Doe")

(defvar d12frosted/org-author-email
  "example@domain.com")
