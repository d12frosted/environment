;;; config.el --- d12frosted Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Boris Buliga & Contributors
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar d12/org-home-path (concat user-home-directory "org/")
  "Directory where org files are stored.
   All org files in this directory and all subdirectories will be
   used as agenda files for org agenda. If you want to ignore
   some files, checkout 'd12/org-agenda-ignore-dirs variable.
   Currently you can ignore files only by putting them into
   ignored directory. Warning! Don't forget to add trailing slash
   at the end.")

(defvar d12/org-agenda-ignore-dirs
  '("temporary/"
    "tmp/")
  "List of directories to be ignored for agenda. Every path
   should be relative to d12/org-home-path and must contain
   trailing slash.")

(defvar d12/time-format
  "%H:%M:%S"
  "Format for 'd12/insert-time function.")

(defvar d12/date-format
  "%d %B %Y, %A"
  "Format for 'd12/insert-date function.")

(defvar d12/org-default-title
  "Yet another org file"
  "Default title for org files. Is used by 'd12/org-guess-title
   when it failt to get the ttitle from buffer name.")

(defvar d12/org-author-name
  user-full-name
  "Author name (used in 'd12/org-new-file-template function.)")

(defvar d12/org-author-email
  user-mail-address
  "Author email (used in 'd12/org-new-file-template function.)")

(defvar d12/dir-settings-file
  "settings.el"
  "File containing dir settings.")

(spacemacs/declare-prefix "." "org")
(spacemacs/declare-prefix ".i" "insert")
