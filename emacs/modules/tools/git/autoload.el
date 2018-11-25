;;; tools/git/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 25 Nov 2018
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

;;;###autoload
(defun +git*update-header-line (revision)
  "Show revision details in the header-line, instead of the
minibuffer."
  (let* ((date-relative (nth 3 revision))
         (date-full (nth 4 revision))
         (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
         (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
    (setq header-line-format
          (format "%s%s [%s (%s)]"
                  (propertize author 'face 'git-timemachine-minibuffer-author-face)
                  (propertize (concat " " sha-or-subject " ") 'face 'git-timemachine-minibuffer-detail-face)
                  date-full date-relative))))
