;;; lib-brb-event.el --- Barberry Garden event-related helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
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
;;; Code:

(require 'org-ml)
(require 'vulpea)
(require 'lib-list)
(require 'lib-vino-stats)
(require 'lib-brb-ledger)
(require 'yaml)

;; * Configuration

(defvar brb-event-narrator-id "bc8aa837-3348-45e6-8468-85510966527a")

;; * Event selection

;;;###autoload
(defun brb-event-select ()
  "Interactively select an event note."
  (let* ((tags '("wine" "event"))
         (event (when (and (eq major-mode 'org-mode)
                           (buffer-file-name))
                  (when-let* ((id (save-excursion
                                    (goto-char (point-min))
                                    (org-id-get)))
                              (note (vulpea-db-get-by-id id)))
                    (when (--every-p (-contains-p (vulpea-note-tags note) it) tags)
                      note)))))
    (vulpea-select-from
     "Event"
     (--filter
      (= 0 (vulpea-note-level it))
      (vulpea-db-query-by-tags-every tags))
     :require-match t
     :initial-prompt (when event (vulpea-note-title event)))))

;;;###autoload
(defun brb-events-from-range (range)
  "Return list of events in time RANGE."
  (->> (vulpea-db-query-by-tags-every '("wine" "event" "barberry/public"))
       (--filter (= 0 (vulpea-note-level it)))
       (--filter (let ((date (vulpea-utils-with-note it
                               (vulpea-buffer-prop-get "date"))))
                   (and (org-time>= date (nth 0 range))
                        (org-time< date (nth 1 range)))))
       (--sort (org-time< (vulpea-utils-with-note it
                            (vulpea-buffer-prop-get "date"))
                          (vulpea-utils-with-note other
                            (vulpea-buffer-prop-get "date"))))))

;; * Wines

;;;###autoload
(defun brb-event-wines (event)
  "Return list of wines from EVENT."
  (vulpea-note-meta-get-list event "wines" 'note))

(defun brb-event-wines--prices (event)
  "Return prices of wines from EVENT.

Result is a properly list (:public :real), where each value is a
list of prices (from the first to the last wine)."
  (vulpea-utils-with-note event
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "#\\+name: data")
      (re-search-forward "^|")
      (goto-char (line-beginning-position))
      (let* ((raw (->> (org-table-to-lisp)
                       (--remove (eq 'hline it))
                       (-map #'cdr)
                       (--map (--map (substring-no-properties it) it))
                       (--filter (seq-contains-p '("price" "price real") (car it)))))
             (public (--find (string-equal "price" (car it)) raw))
             (real (--find (string-equal "price real" (car it)) raw)))
        (list
         :public (->> public (cdr) (-map #'string-to-number))
         :real (->> real (cdr) (-map #'string-to-number)))))))

;; * Participants

;;;###autoload
(defun brb-event-participants (event)
  "Return list of participants from EVENT."
  (vulpea-note-meta-get-list event "participants" 'note))

(defun brb-event--participants ()
  "Return list of participants from the currently visited event."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format org-complex-heading-regexp-format (regexp-quote "Preparation")))
    (search-forward "1. ")
    (beginning-of-line)
    (->> (org-list-struct)
         (--map
          (save-excursion
            (goto-char (+ (nth 0 it) (length (nth 2 it))))
            (let ((e (org-element-context)))
              (cond
               ((eq 'link (org-element-type e))
                (org-element-property :path e))))))
         (-filter #'identity)
         (vulpea-db-query-by-ids)
         (--remove (vulpea-note-primary-title it)))))

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
   (personal . (((item . str)
                 (price . num)
                 (orders . (((participant . id)
                             (amount . num)))))))
   (participants . ((id . id)
                    (fee . num)))
   (wines . (((id . id)
             (price-public . num)
             (price-real . num)
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
(defun brb-event-summary (event)
  "Return score summary of EVENT."
  (let* ((data (brb-event-data-read event))
         (wines (brb-event-wines event))
         (participants (brb-event-participants event))
         (weight-def 2)
         (weights (->> participants
                       (--map
                        (let ((weight (or (vulpea-note-meta-get
                                           it
                                           "tasting level"
                                           'number)
                                          weight-def)))
                          `((participant . ,(vulpea-note-id it))
                            (weight . ,(calc-from-number (* weight weight))))))))
         (wines-data (->>
                      wines
                      (-map
                       (lambda (wine)
                         (let ((data (--find (string-equal (vulpea-note-id wine) (alist-get 'id it))
                                             (alist-get 'wines data))))
                           (unless data
                             (error "Could not find scores data for %s" (vulpea-note-id wine)))
                           (let* ((scores (->> data
                                               (alist-get 'scores)
                                               (--map (alist-get 'score it))
                                               (--filter it)
                                               (-map #'calc-from-number)))
                                  (amean (when scores
                                           (calc-to-number (apply #'calcFunc-vmean scores))))
                                  (rms (when scores
                                         (calc-to-number (calcFunc-rms (apply #'calcFunc-vec scores)))))
                                  (weights-sum (->> weights
                                                    (--filter (-contains-p
                                                               (->> data
                                                                    (alist-get 'scores)
                                                                    (--filter (alist-get 'score it))
                                                                    (--map (alist-get 'participant it)))
                                                               (alist-get 'participant it)))
                                                    (--map (alist-get 'weight it))
                                                    (apply #'calcFunc-vec)
                                                    (calcFunc-vsum)
                                                    (calc-to-number)))
                                  (wtotal (->> data
                                               (alist-get 'scores)
                                               (--filter (alist-get 'score it))
                                               (--map
                                                (let* ((pid (alist-get 'participant it))
                                                       (weight-data (--find
                                                                     (string-equal pid (alist-get 'participant it))
                                                                     weights)))
                                                  (unless weight-data
                                                    (error "Could not find weight data for participant %s" pid))
                                                  (calcFunc-mul (assoc-default 'weight weight-data)
                                                                (assoc-default 'score it))))
                                               (apply #'calcFunc-vec)
                                               (calcFunc-vsum)))
                                  (wavg (when scores (/ wtotal weights-sum)))
                                  (price (alist-get 'price-public data))
                                  (qpr (brb-qpr price wavg))
                                  (sdev (when scores
                                          (calc-to-number (apply #'calcFunc-vpvar scores))))
                                  (fav (->> data
                                            (alist-get 'scores)
                                            (--map (alist-get 'sentiment it))
                                            (--count (string-equal "favourite" it))))
                                  (out (->> data
                                            (alist-get 'scores)
                                            (--map (alist-get 'sentiment it))
                                            (--count (string-equal "outcast" it))))
                                  (pscores (->> data
                                                (alist-get 'scores)
                                                (--map
                                                 (let* ((pid (alist-get 'participant it))
                                                        (p (--find (string-equal pid (vulpea-note-id it))
                                                                   participants)))
                                                   `((participant . ,p)
                                                     (score . ,(alist-get 'score it))
                                                     (sentiment . ,(alist-get 'sentiment it))))))))
                             `((wine . ,wine)
                               (ignore-scores . ,(alist-get 'ignore-scores data))
                               (amean . ,amean)
                               (rms . ,rms)
                               (wavg . ,wavg)
                               (sdev . ,sdev)
                               (fav . ,fav)
                               (out . ,out)
                               (price . ,price)
                               (qpr . ,qpr)
                               (scores . ,pscores))))))))
         (prices (->> data
                      (alist-get 'wines)
                      (--remove (alist-get 'ignore-scores it))
                      (--map (alist-get 'price-public it))
                      (--filter it)))
         (wines-price-total (when prices (-sum prices)))
         (wines-price-harmonic (when prices
                                 (->> prices
                                      (-map #'calc-from-number)
                                      (apply #'calcFunc-vec)
                                      (calcFunc-vhmean)
                                      (calc-to-number))))
         (wines-price-median (when prices
                               (->> prices
                                    (-map #'calc-from-number)
                                    (apply #'calcFunc-vec)
                                    (calcFunc-vmedian)
                                    (calc-to-number))))
         (event-rms (->> wines-data
                         (--remove (alist-get 'ignore-scores it))
                         (--map (alist-get 'rms it))
                         (--filter it)
                         (-map #'calc-from-number)
                         (apply #'calcFunc-vec)
                         (calcFunc-rms)
                         (calc-to-number)))
         (event-wavg (->> wines-data
                         (--remove (alist-get 'ignore-scores it))
                         (--map (alist-get 'wavg it))
                         (--filter it)
                         (-map #'calc-from-number)
                         (apply #'calcFunc-vec)
                         (calcFunc-rms)
                         (calc-to-number)))
         (event-qpr (when prices (brb-qpr wines-price-harmonic event-wavg))))
    `((wines . ,wines-data)
      (wines-price-total . ,wines-price-total)
      (wines-price-harmonic . ,wines-price-harmonic)
      (wines-price-median . ,wines-price-median)
      (rms . ,event-rms)
      (wavg . ,event-wavg)
      (qpr . ,event-qpr))))

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
         (convives (->> (brb-events-from-range (list "2000-01-01" (format-time-string "%Y-%m-%d" (current-time))))
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
                             (if-let ((r (--find (s-prefix-p it (nth 1 parts)) rules)))
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
  (--each (brb-events-from-range (list "2000-01-01" (format-time-string "%Y-%m-%d" (current-time))))
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
