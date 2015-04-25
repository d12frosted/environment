;;; extensions.el --- d12frosted-org Layer extensions File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-org-pre-extensions
  '()
  "List of all extensions to load before the packages.")

(defvar d12frosted-org-post-extensions
  '(org-journal)
  "List of all extensions to load after the packages.")

(defun d12frosted-org/init-org-journal ()
  "Initialize org-journal package."
  (use-package org-journal
    :defer 1
    :init
    :config
    (global-unset-key (kbd "C-c C-j"))

    (add-to-list 'auto-mode-alist '(".*/[0-9]*-[0-9]*-[0-9]*$" . org-mode))

    (evil-leader/set-key "ojc" 'calendar)
    (evil-leader/set-key "ojn" 'org-journal-new-entry)
    (evil-leader/set-key "ojv" 'org-journal-visit-entry)

    (setq org-journal-dir (s-concat d12frosted/org-home-path "journal/")
          org-journal-date-format "%d %B %Y, %A"
          org-journal-file-format "%Y-%m-%d"
          org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
          org-journal-do-hide-entries-on-new nil)))
