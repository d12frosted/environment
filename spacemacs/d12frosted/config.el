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
   All org files in this directory and all subdirectories will be used as agenda
   files for org agenda. If you want to ignore some files, checkout
   'd12/org-agenda-ignore-dirs variable. Currently you can ignore files
   only by putting them into ignored directory.
   Warning! Don't forget to add trailing slash at the end.")

(defvar d12/org-agenda-ignore-dirs
  '("temporary/"
    "tmp/")
  "List of directories to be ignored for agenda. Every path should be relative
   to d12/org-home-path and must contain trailing slash.")

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

(defvar d12-mu4e/private-config-path
  (concat user-home-directory "mu4e-configs.el")
  "Path to mu4e private configs.")

(defvar d12-mu4e/installation-path nil
  "Path to mu4e. Depends on installation.")

(defvar d12-mu4e/folders-alist
  '(("example"
     (mu4e-drafts-folder "/example/Drafts")
     (mu4e-sent-folder "/example/Sent")
     (mu4e-trash-folder "/example/Trash")
     (mu4e-refile-folder "/example/Archive"))))

(defvar d12-mu4e/accounts-alist
  '(("example"
     (mu4e-sent-messages-behavior delete)
     (user-mail-address "example@example.com")
     (user-full-name  "John Doe")
     (mu4e-compose-signature "Sent with love using emacs and mu4e."))))

(defvar d12-mu4e/default-account "example")
(defvar d12-mu4e/update-interval 60)
(defvar d12-mu4e/maildir-path
  (concat user-home-directory "Maildir"))

(spacemacs/declare-prefix "." "org")
(spacemacs/declare-prefix ".i" "insert")
