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

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'.

And don't forget to refresh version control in all buffers of
current workspace."
  (interactive)
  (quit-window)
  (unless (cdr
           (delq nil
                 (mapcar (lambda (win)
                           (with-selected-window win
                             (eq major-mode 'magit-status-mode)))
                         (window-list))))
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
    (dolist (buffer (+workspace-buffer-list))
      (with-current-buffer buffer
        (when (fboundp 'vc-refresh-state)
          (vc-refresh-state))
        (when (fboundp '+version-control|update-git-gutter)
          (+version-control|update-git-gutter))))))

(defun +magit--kill-buffer (buffer)
  "Gracefully kill magit BUFFER.

If any alive process is related to this BUFFER, wait for 5
seconds before nuking BUFFER and the process. If it's dead -
don't wait at all."
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (let ((process (get-buffer-process buffer)))
      (if (not (processp process))
          (kill-buffer buffer)
        (with-current-buffer buffer
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buffer)
            (kill-process process)
            (kill-buffer buffer)))))))
