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
(require 'lib-vino-stats)



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
(defun brb-event--from-range (range)
  "Return list of events in time RANGE."
  (->> (vulpea-db-query-by-tags-every '("wine" "event"))
       (--filter (= 0 (vulpea-note-level it)))
       (--remove (vulpea-note-tagged-any-p it "external"))
       (--filter (let ((date (vulpea-utils-with-note it
                               (vulpea-buffer-prop-get "date"))))
                   (and (org-time>= date (nth 0 range))
                        (org-time< date (nth 1 range)))))
       (--sort (org-time< (vulpea-utils-with-note it
                            (vulpea-buffer-prop-get "date"))
                          (vulpea-utils-with-note other
                            (vulpea-buffer-prop-get "date"))))))



;;;###autoload
(defun brb-event-wines (event)
  "Return list of wines from EVENT."
  (vulpea-utils-with-note event
    (let ((bound (save-excursion
                   (goto-char (point-min))
                   (re-search-forward org-heading-regexp)
                   (beginning-of-line)
                   (point))))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "1. " bound 'no-error)
          (beginning-of-line)
          (->> (org-ml-parse-element-at (point))
               (org-ml-get-children)
               (-map #'org-ml-item-get-paragraph)
               (-map #'car)
               (--map (org-ml-get-property :path it))
               (-map #'vulpea-db-get-by-id)))))))



;;;###autoload
(defun brb-event-participants (event)
  "Return list of participants from EVENT."
  (vulpea-utils-with-note event
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (format org-complex-heading-regexp-format (regexp-quote "Preparation")))
      (search-forward "1. ")
      (beginning-of-line)
      (->> (org-ml-parse-element-at (point))
           (org-ml-get-children)
           (-map #'org-ml-item-get-paragraph)
           (-map #'car)
           (--map (org-ml-get-property :path it))
           (vulpea-db-query-by-ids)
           (--remove (vulpea-note-primary-title it))))))



;;;###autoload
(defun brb-event-score-summary (event)
  "Return score summary of EVENT."
  (vulpea-utils-with-note event
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "#\\+results: summary")
      (forward-line 1)
      (let* ((raw (->> (org-table-to-lisp)
                       (--remove (eq 'hline it))
                       (--map (--map (->> it
                                          (substring-no-properties)
                                          (s-replace-regexp "[*+]" ""))
                                     it))))
             (headline (car raw)))
        (--map (--zip-with (cons it (string-to-number other))
                           (cdr headline)
                           (cdr it))
               (cdr raw))))))



;;;###autoload
(defun brb-event-stats (&optional frame)
  "Display events stats for a time FRAME."
  (interactive)
  (when-let* ((frame (or
                      frame
                      (intern
                       (completing-read
                        "Time frame: " (cons 'custom vino-stats-time-frames)
                        nil 'require-match))))
              (range (pcase frame
                       (`custom (list
                                 (org-read-date nil nil nil "From (inclusive)")
                                 (org-read-date nil nil nil "To (exclusive)")))
                       (_ (vino-stats--time-frame-range frame))))
              (events (brb-event--from-range range))
              (participants (->> events
                                 (--map (brb-event-participants it))
                                 (-reduce-from #'-union nil)))
              (wines-all (->> events
                              (--map (brb-event-wines it))
                              (-flatten)))
              (wines (-distinct wines-all))

              (grapes-all (->> wines-all
                               (--map (vulpea-note-meta-get-list it "grapes" 'note))
                               (-flatten)))
              (grapes (-distinct grapes-all))

              (roas-all (->> wines-all
                             (--map (or (vulpea-note-meta-get it "region" 'note)
                                        (vulpea-note-meta-get it "appellation" 'note)))
                             (-flatten)))
              (roas (-distinct roas-all))

              (countries-all (->> wines-all
                                  (--map (vulpea-note-meta-get
                                          (or (vulpea-note-meta-get it "region" 'note)
                                              (vulpea-note-meta-get it "appellation" 'note))
                                          "country" 'note))
                                  (-flatten)))
              (countries (-distinct countries-all)))
    (buffer-display-result-with "*brb-stats*"
      (format "Stats for period from %s to %s"
              (propertize (nth 0 range) 'face 'bold)
              (propertize (nth 1 range) 'face 'bold))
      ""

      (propertize (format "Events (%s)" (seq-length events)) 'face 'bold)
      ""
      (string-table
       :header '("date" "event" "participants" "wines" "amean" "rms" "price")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data
       (-concat
        (--map
         (let ((summary (brb-event-score-summary it))
               (wines (brb-event-wines it)))
           (list (vulpea-utils-with-note it
                   (vulpea-buffer-prop-get "date"))
                 it
                 (seq-length (brb-event-participants it))
                 (seq-length wines)
                 (format "%.4f"
                         (/ (->> summary
                                 (--map (assoc-default "amean" it))
                                 (-sum))
                            (seq-length wines)))
                 (format "%.4f"
                         (/ (->> summary
                                 (--map (assoc-default "rms" it))
                                 (-sum))
                            (seq-length wines)))
                 (->> summary
                      (--map (assoc-default "price" it))
                      (-sum))))
         events)
        '(sep)
        `((""
           ""
           ,(--reduce-from
             (+ acc (seq-length (brb-event-participants it)))
             0 events)
           ,(--reduce-from
             (+ acc (seq-length (brb-event-wines it)))
             0 events)
           ""
           ""
           ""))))
      ""

      (propertize (format "Participants (%s)" (seq-length participants)) 'face 'bold)
      ""
      (string-join
       (--map (format "- %s" (string-from it)) participants)
       "\n")
      ""

      (propertize (format "Wines (%s)" (seq-length wines)) 'face 'bold)
      ""
      (string-join
       (--map (format "- %s" (string-from it)) wines)
       "\n")
      ""
      
      (string-table
       :header '("date" "event" "producer" "wine" "vintage" "amean" "rms" "sdev" "price" "qpr")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :pad-type '(right right right right left left left left left left)
       :width '(full 20 20 20 full full full full full full)
       :data
       (->> events
            (--map
             (let ((summary (brb-event-score-summary it))
                   (event it))
               (->> (brb-event-wines it)
                    (--map-indexed (list
                                    (vulpea-utils-with-note event
                                      (vulpea-buffer-prop-get "date"))
                                    event
                                    (vulpea-note-meta-get it "producer" 'note)
                                    (vulpea-note-meta-get it "name")
                                    (or (vulpea-note-meta-get it "vintage") "NV")
                                    (assoc-default "amean" (nth it-index summary))
                                    (assoc-default "rms" (nth it-index summary))
                                    (assoc-default "sdev" (nth it-index summary))
                                    (assoc-default "price" (nth it-index summary))
                                    (assoc-default "QPR" (nth it-index summary)))))))
            (-flatten-n 1)
            (--sort (> (nth 6 it)
                       (nth 6 other)))))
      ""

      (propertize (format "Grapes (%s)" (seq-length grapes)) 'face 'bold)
      ""
      (string-table
       :header '("grape" "count")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data
       (->> grapes
            (--map
             (list it (-count (lambda (other)
                                (string-equal
                                 (vulpea-note-id it)
                                 (vulpea-note-id other)))
                              grapes-all)))
            (--sort (> (nth 1 it)
                       (nth 1 other)))))
      ""

      (propertize (format "Countries (%s)" (seq-length countries)) 'face 'bold)
      ""
      (string-table
       :header '("grape" "count")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data
       (->> countries
            (--map
             (list it (-count (lambda (other)
                                (string-equal
                                 (vulpea-note-id it)
                                 (vulpea-note-id other)))
                              countries-all)))
            (--sort (> (nth 1 it)
                       (nth 1 other)))))
      ""

      (propertize (format "Regions (%s)" (seq-length roas)) 'face 'bold)
      ""
      (string-table
       :header '("grape" "count")
       :header-sep "-"
       :header-sep-start "|-"
       :header-sep-conj "-+-"
       :header-sep-end "-|"
       :row-start "| "
       :row-end " |"
       :sep " | "
       :data
       (->> roas
            (--map
             (list it (-count (lambda (other)
                                (string-equal
                                 (vulpea-note-id it)
                                 (vulpea-note-id other)))
                              roas-all)))
            (--sort (> (nth 1 it)
                       (nth 1 other)))))
      "")))



(provide 'lib-brb-event)
;;; lib-brb-event.el ends here
