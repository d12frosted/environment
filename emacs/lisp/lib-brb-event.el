;;; lib-brb-event.el --- Barberry Garden event-related helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Sep 2022
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Single wine event management: data extraction, participant handling,
;; wine selection, scoring, and event buffer display.
;;
;;; Code:

(require 'org-ml)
(require 'vulpea)
(require 'lib-list)
(require 'brb-event)
(require 'yaml)

;; * Data (non-meta)

(defun brb-event--data-file (event)
  "Return path to data file of EVENT."
  (file-name-with-extension (vulpea-note-path event) "data.el"))

;;;###autoload
(defun brb-event-data-read (event)
  "Read data for EVENT.

The result contains all the extra data for EVENT that can't be
stored as metadata in vulpea-note. It has the following
structure:

  ((planned-participants . num)
   (shared . (((item . str)
               (amount . num)
               (price . num))))
   (expense-wines . (((id . id)
                      (amount . num)
                      (price . num))))
   (personal . (((item . str)
                 (price . num)
                 (orders . (((participant . id)
                             (amount . num)))))))
   (participants . (((id . id)
                     (fee . num))))
   (wines . (((id . id)
              (price-public . num)
              (price-real . num)
              (price-asking . num)
              (participants . (id))
              (type . str)
              (ignore-scores . bool)
              (scores . (((participant . id)
                          (score . num)
                          (sentiment . str))))))))"
  (let ((file (brb-event--data-file event)))
    (when (file-exists-p file)
      (with-temp-buffer
        (condition-case nil
	          (progn
	            (insert-file-contents file)
              (read (current-buffer)))
	        (error
	         (message "Could not read data from %s" file)))))))

;;;###autoload
(defun brb-event-data-write (event data)
  "Write DATA for EVENT."
  (let ((file (brb-event--data-file event)))
    (with-temp-file file
      (let ((print-level nil)
	          (print-length nil))
	      (pp data (current-buffer))))))

;; * Summary

;;;###autoload

;; * Content management

;;;###autoload
(defun brb-event-insert-participant-link ()
  "Select a participant and insert link to it.

Uses public name as description."
  (interactive)
  (let ((note (vulpea-select-from
               "Participant"
               (vulpea-db-query-by-tags-every '("people"))
               :require-match t)))
    (insert (org-link-make-string
             (concat "id:" (vulpea-note-id note))
             (vulpea-note-meta-get note "public name")))))

(defun brb-event-wine-edit ()
  "Edit event wine at point."
  (interactive)
  (save-excursion
    (unless (looking-at org-heading-regexp)
      (outline-previous-heading)
      (->> (org-ml-parse-this-subtree)
           (org-ml-match '(:any * src-block))
           (car)
           (org-ml-get-property :value)
           (yaml-parse-string)))))

;; * Maintenance

(defun brb-events-assign-public-names ()
  "Assign public names to all public events."
  (interactive)
  (let* ((rules '("Kh" "Sh" "Yu" "Ya" "Tkh" "Ch" "Zh" "Shch"))
         (convives (->> (brb-events-from-range "2000-01-01" (format-time-string "%Y-%m-%d" (current-time)))
                        (--map (brb-event-participants it))
                        (-flatten-n 1)
                        (-distinct)
                        (--sort (string< (vulpea-note-title it) (vulpea-note-title other)))))
         (public-names))
    (--each convives
      (let* ((parts (s-split-words (vulpea-note-title it)))
             (name (cond
                    ((= (seq-length parts) 2)
                     (concat (nth 0 parts)
                             " "
                             (if-let* ((r (--find (s-prefix-p it (nth 1 parts)) rules)))
                                 r
                               (s-left 1 (nth 1 parts)))))
                    (t (read-string
                        (format "%s has a strangely shaped name, give it a public name manually: "
                                (vulpea-note-title it)))))))
        (when (-contains-p public-names name)
          (setq name
                (read-string (format "%s can not take %s as short name, as it is taken, provide new one (%s): "
                                     (vulpea-note-title it)
                                     name
                                     (vulpea-note-meta-get it "public name"))
                             nil nil (vulpea-note-meta-get it "public name"))))
        (push name public-names)
        (vulpea-utils-with-note it
          (vulpea-buffer-meta-set "public name" name)
          (save-buffer)
          (kill-buffer))))))

(defun brb-events-execute-blocks ()
  "Execute code blocks in all public events."
  (interactive)
  (--each (brb-events-from-range "2000-01-01" (format-time-string "%Y-%m-%d" (current-time)))
    (--each (seq-reverse
             (org-element-map
                 (org-element-parse-buffer 'element)
                 'src-block
               (lambda (h)
                 (org-element-property :begin h))))
      (goto-char it)
      (let ((org-confirm-babel-evaluate nil))
        (save-excursion
          (silenzio
           (funcall-interactively #'org-babel-execute-src-block)))))
    (save-buffer)
    (kill-buffer)))

(provide 'lib-brb-event)
;;; lib-brb-event.el ends here
