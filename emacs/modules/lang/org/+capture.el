;;; lang/org/+capture.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
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

(add-hook 'org-load-hook #'+org|setup-capture)

(defvar +capture-inbox-file "inbox.org"
  "The path to the inbox file.

It is relative to `org-directory', unless it is absolute.")

(defvar +capture-journal-file "notes/journal.org"
  "The path to the journal file.

It is relative to `org-directory', unless it is absolute.")

(defvar org-capture-templates
  '(("t" "todo" plain (file +capture-inbox-file)
     "* TODO %?\n%U\n" :clock-in t :clock-resume t)
    
    ("j" "Journal" entry (file+datetree+prompt +capture-journal-file)
     "* %?\n%U\n" :clock-in t :clock-resume t)

    ("n" "note" entry (file +capture-inbox-file)
     "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

    ("m" "Meeting" entry (file +capture-inbox-file)
     "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)))

(defun +org|setup-capture ()
  "Setup `org-capture'."
  (dolist (var '(+capture-inbox-file
                 +capture-journal-file))
    (set var (expand-file-name (symbol-value var) org-directory)))
  (unless org-default-notes-file
    (setq org-default-notes-file +capture-inbox-file)))
