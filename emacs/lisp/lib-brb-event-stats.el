;;; lib-brb-event-stats.el --- Display event stats -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2023, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Mar 2023
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

(require 'vulpea)
(require 'lib-vino-stats)
(require 'lib-hash-table)
(require 'lib-brb-event)
(require 'lib-brb-event-plan)

;;;###autoload
(defun brb-event-stats (&optional frame)
  "Display events stats for a time FRAME."
  (interactive)
  (brb-event-stats-for
   :frame frame
   :buffer-name "*brb-stats*"
   :events-filter (lambda (it) (not (vulpea-note-tagged-any-p it "external")))
   :include-gain t))

;;;###autoload
(defun brb-event-stats-external (&optional frame)
  "Display external events stats for a time FRAME."
  (interactive)
  (brb-event-stats-for
   :frame frame
   :buffer-name "*brb-stats-external*"
   :events-filter (lambda (it) (vulpea-note-tagged-any-p it "external"))))

;;;###autoload
(defun brb-event-stats-vova (&optional frame)
  "Display Vova's events stats for a time FRAME."
  (interactive)
  (brb-event-stats-for
   :frame frame
   :buffer-name "*brb-stats-vova*"
   :events-filter (lambda (it) (vulpea-note-tagged-all-p it "external" "@VovaUlianov"))))

(cl-defun brb-event-stats-for (&key frame
                                    events-filter
                                    buffer-name
                                    include-gain)
  "Display events stats for FRAME.

Events are filtered using EVENTS-FILTER.

Optionally, one may provide BUFFER-NAME.

When INCLUDE-GAIN is non-nil, the gain is included in the summary."
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
              (events (->> (brb-events-from-range range)
                           (-filter events-filter))))
    (let* ((events-summary (hash-table-from events
                             :key-fn #'vulpea-note-id
                             :value-fn (lambda (event _) (brb-event-summary event))))

           (participants-all (->> events
                                  (--map (brb-event-participants it))
                                  (-flatten)))
           (participants (-distinct participants-all))

           (wines-all (->> events
                           (--map (brb-event-wines it))
                           (-flatten)))
           (wines (-distinct wines-all))

           (grapes-all (->> wines-all
                            (--map (vulpea-note-meta-get-list it "grapes" 'note))
                            (-flatten)))
           (grapes (->> grapes-all
                        (-map #'vulpea-note-id)
                        (vulpea-db-query-by-ids)
                        (--filter (null (vulpea-note-primary-title it)))))

           (producers-all (->> wines-all
                               (--map (vulpea-note-meta-get it "producer" 'note))
                               (-flatten)))
           (producers (-distinct producers-all))

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
           (countries (-distinct countries-all))
           (gains (when include-gain (--map (alist-get 'balance-real
                                                       (brb-event-statement it :balances (make-hash-table)))
                                            events))))
      (buffer-display-result-with (or buffer-name "*brb-stats*")
        (format "Stats for period from %s to %s"
                (propertize (nth 0 range) 'face 'bold)
                (propertize (nth 1 range) 'face 'bold))
        ""

        (propertize (format "Events (%s)" (seq-length events)) 'face 'bold)
        ""
        (string-table
         :header (if include-gain
                     '("date" "event" "convives" "wines" "wavg" "price" "gain")
                   '("date" "event" "convives" "wines" "wavg" "price"))
         :pad-type '(right right left left left left left)
         :width '(nil 48 nil nil nil nil nil)
         :header-sep "-"
         :header-sep-start "|-"
         :header-sep-conj "-+-"
         :header-sep-end "-|"
         :row-start "| "
         :row-end " |"
         :sep " | "
         :data
         (-concat
          (--map-indexed
           (condition-case nil
               (let* ((summary (gethash (vulpea-note-id it) events-summary))
                      (wines (brb-event-wines it)))
                 (list (vulpea-utils-with-note it
                         (org-read-date nil nil (vulpea-buffer-prop-get "date")))
                       it
                       (seq-length (brb-event-participants it))
                       (seq-length wines)
                       (if-let ((v (alist-get 'wavg summary)))
                           (format "%.4f" v)
                         "----")
                       (alist-get 'wines-price-total summary)
                       (when include-gain (nth it-index gains))))
             ((debug error) (message "Failed to calculate summary of '%s'" (vulpea-note-title it))))
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
             ,(when include-gain (-sum gains))))))
        ""

        (propertize (format "Participants (%s)" (seq-length participants)) 'face 'bold)
        ""
        (string-table
         :header '("participant" "count")
         :header-sep "-"
         :header-sep-start "|-"
         :header-sep-conj "-+-"
         :header-sep-end "-|"
         :row-start "| "
         :row-end " |"
         :sep " | "
         :data
         (->> participants
              (--map
               (list it (-count (lambda (other)
                                  (string-equal
                                   (vulpea-note-id it)
                                   (vulpea-note-id other)))
                                participants-all)))
              (--sort (> (nth 1 it)
                         (nth 1 other)))))
        ""

        (propertize (format "Wines (%s)" (seq-length wines)) 'face 'bold)
        ""
        (string-join
         (--map (format "- %s" (string-from it)) wines)
         "\n")
        ""

        (string-table
         :header '("date" "event" "producer" "wine" "year" "wavg" "sdev" "price" "qpr")
         :header-sep "-"
         :header-sep-start "|-"
         :header-sep-conj "-+-"
         :header-sep-end "-|"
         :row-start "| "
         :row-end " |"
         :sep " | "
         :pad-type '(right right right right left left left left left)
         :width '(full 16 18 20 full full full full full)
         :data
         (->> events
              (--map
               (let ((summary (alist-get 'wines (gethash (vulpea-note-id it) events-summary)))
                     (event it))
                 (->> (brb-event-wines it)
                      (--map-indexed (list
                                      (vulpea-utils-with-note event
                                        (org-read-date nil nil (vulpea-buffer-prop-get "date")))
                                      event
                                      (vulpea-note-meta-get it "producer" 'note)
                                      (vulpea-buttonize it (lambda (it) (vulpea-note-meta-get it "name")))
                                      (or (vulpea-note-meta-get it "vintage") "NV")
                                      (if-let ((wavg (->> summary
                                                          (nth it-index)
                                                          (alist-get 'wavg))))
                                          (format "%.4f" wavg) "-")
                                      (if-let ((sdev (->> summary
                                                          (nth it-index)
                                                          (alist-get 'sdev))))
                                          (format "%.4f" sdev) "-")
                                      (assoc-default 'price (nth it-index summary))
                                      (if-let ((qpr (assoc-default 'qpr (nth it-index summary))))
                                          (format "%.4f" qpr) "-"))))))
              (-flatten-n 1)
              (--sort (string> (nth 5 it)
                               (nth 5 other)))))
        ""

        (propertize (format "Producers (%s)" (seq-length producers)) 'face 'bold)
        ""
        (string-table
         :header '("producer" "count")
         :pad-type '(right left)
         :header-sep "-"
         :header-sep-start "|-"
         :header-sep-conj "-+-"
         :header-sep-end "-|"
         :row-start "| "
         :row-end " |"
         :sep " | "
         :data
         (->> producers
              (--map
               (list it (-count (lambda (other)
                                  (string-equal
                                   (vulpea-note-id it)
                                   (vulpea-note-id other)))
                                producers-all)))
              (--sort (> (nth 1 it)
                         (nth 1 other)))))
        ""

        (propertize (format "Grapes (%s)" (seq-length grapes)) 'face 'bold)
        ""
        (string-table
         :header '("grape" "count")
         :pad-type '(right left)
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
         :header '("country" "count")
         :pad-type '(right left)
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
         :header '("region" "count")
         :pad-type '(right left)
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
        ""))))

(provide 'lib-brb-event-stats)
;;; lib-brb-event-stats.el ends here
