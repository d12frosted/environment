;;; lib-brb-events.el --- A tool for viewing and planning events for barberry garden -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 15 Jun 2024
;;
;; URL: https://github.com/d12frosted/
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
(require 'lib-plist)
(require 'lib-vino-stats)
(require 'lib-brb-event)
(require 'lib-brb-event-plan)
(require 'lib-brb-ledger)
(require 'lib-hash-table)

;;;###autoload
(defun brb-events (&optional arg)
  "Display UI for managing events.

By default this full year is used as a time frame. Pass a universal
ARG to override and query for specific frame."
  (interactive "P")
  (let* ((buffer (buffer-generate "*barberry garden events*" 'unique))
         (frame (if arg nil 'this-year-full))
         (frame (or
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
         (data (events-data--create :buffer buffer
                                    :tab 'overview
                                    :filter 'internal
                                    :frame frame
                                    :range range)))
    (events-data-reload-events data)
    (brb-events--propagate-new data)
    (switch-to-buffer buffer)))

;; * Events Data

(cl-defstruct (events-data (:constructor events-data--create)
                           (:copier nil))
  buffer
  tab

  range
  frame

  events-all
  filter
  events

  statement
  summary)

(cl-defmethod events-data-reload-events ((x events-data))
  "Reload events in X."
  (let* ((events (-concat (brb-events-from-range (events-data-range x))
                          (brb-events-without-date))))
    (setf (events-data-events-all x) events
          (events-data-summary x) nil
          (events-data-statement x) nil)
    (events-data-set-filter x (events-data-filter x))))

(cl-defmethod events-data-load-summary-maybe ((x events-data))
  "Load data into summary slot of X if its missing."
  (unless (events-data-summary x)
    (setf (events-data-summary x)
          (hash-table-from (events-data-events-all x)
            :key-fn #'vulpea-note-id
            :value-fn (lambda (event _) (brb-event-summary event))))))

(cl-defmethod events-data-load-statement-maybe ((x events-data))
  "Load data into statement slot of X if its missing."
  (unless (events-data-statement x)
    (setf (events-data-statement x)
          (hash-table-from (events-data-events-all x)
            :key-fn #'vulpea-note-id
            :value-fn (lambda (event _)
                        (brb-event-statement event :balances (make-hash-table)))))))

(cl-defmethod events-data-set-filter ((x events-data) filter)
  "Set FILTER in X."
  (setf (events-data-events x) (pcase filter
                                 (`all (events-data-events-all x))
                                 (`internal (--filter
                                             (not (vulpea-note-tagged-all-p it "external"))
                                             (events-data-events-all x)))
                                 (`external (--filter
                                             (vulpea-note-tagged-all-p it "external")
                                             (events-data-events-all x)))
                                 (_ (user-error "Unexpected filter: %S" filter)))
        (events-data-filter x) filter))

(cl-defmethod events-data-set-tab ((x events-data) tab)
  "Set TAB and reload X."
  (setf (events-data-tab x) tab)
  (brb-events--propagate-new x))

(defconst brb-events-filters '(all internal external))

;; * Buffer building blocks

(defun brb-events--propagate-new (x)
  "Propagate events buffer for X."
  (with-current-buffer (events-data-buffer x)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil)
          (point))
      (setq point (point))
      (silenzio
       (toggle-truncate-lines 1))
      (erase-buffer)

      ;; content
      (brb-events--header x)
      (insert "\n")
      (pcase (events-data-tab x)
        (`overview (brb-events--tab-overview x))
        (`stats (brb-events--tab-stats x)))
      (insert "\n")
      (ws-butler-clean-region (point-min) (point-max))
      (goto-char point))))

(cl-defmethod brb-events--header ((x events-data))
  "Insert header for X."
  (let ((tabs '(overview stats))
        (set-tab (-partial #'events-data-set-tab x))
        (set-filter (-partial
                     (lambda (x filter)
                       (events-data-set-filter x filter)
                       (brb-events--propagate-new x))
                     x)))
    (insert
     (propertize "Barberry Garden Events" 'face 'help-for-help-header) "\n"
     (format "from %s to %s" (nth 0 (events-data-range x)) (nth 1 (events-data-range x))) "\n"
     (mapconcat
      (lambda (filter)
        (if (eq (events-data-filter x) filter)
            (propertize
             (buttonize (concat "[*" (symbol-name filter) "*]") set-filter filter)
             'face 'barberry-theme-face-strong)
          (buttonize (concat "[" (symbol-name filter) "]") set-filter filter)))
      brb-events-filters " ")
     "\n"
     "\n"
     (mapconcat
      (lambda (tab)
        (if (eq (events-data-tab x) tab)
            (propertize
             (buttonize (concat "[*" (symbol-name tab) "*]") set-tab tab)
             'face 'barberry-theme-face-strong)
          (buttonize (concat "[" (symbol-name tab) "]") set-tab tab)))
      tabs " ")
     "\n")))

;; * Overview

(cl-defmethod brb-events--tab-overview ((x events-data))
  "Render overview tab for X."
  (let* ((set-location (-partial
                        (lambda (x id)
                          "Set location of event with ID in X."
                          (let ((location (vulpea-select-from
                                           "Location"
                                           (vulpea-db-query-by-tags-every '("places"))
                                           :require-match t)))
                            (vulpea-utils-with-note (vulpea-db-get-by-id id)
                              (vulpea-buffer-meta-set "location" location)
                              (save-buffer))
                            (events-data-reload-events x)
                            (brb-events--propagate-new x)))
                        x))
         (set-host (-partial
                    (lambda (x id)
                      "Set host  of event with ID in X."
                      (let ((host (vulpea-select-from
                                   "Host"
                                   (vulpea-db-query-by-tags-every '("people"))
                                   :require-match t)))
                        (vulpea-utils-with-note (vulpea-db-get-by-id id)
                          (vulpea-buffer-meta-set "host" host)
                          (save-buffer))
                        (events-data-reload-events x)
                        (brb-events--propagate-new x)))
                    x))
         (set-date (-partial
                    (lambda (x id)
                      "Set date of event with ID in X."
                      (let ((date (with-temp-buffer
                                    (let ((date (org-read-date nil t nil "Date: ")))
                                      (org-insert-timestamp date nil t)
                                      (buffer-substring (point-min) (point-max))))))
                        (vulpea-utils-with-note (vulpea-db-get-by-id id)
                          (vulpea-buffer-prop-set "date" date)
                          (save-buffer))
                        (events-data-reload-events x)
                        (brb-events--propagate-new x)))
                    x))
         (new-event (-partial
                     (lambda (x &rest _)
                       (brb-event-create)
                       (events-data-reload-events x)
                       (brb-events--propagate-new x))
                     x)))
    (insert
     (propertize (format "Events (%d) %s"
                         (seq-length (events-data-events x))
                         (buttonize "[+]" new-event))
                 'face 'outline-1)
     "\n\n"
     (string-table
      :header '("date" "" "" "event" "location" "host" "folks" "wines" "price")
      :pad-type '(right right right right right right left left left)
      :width '(nil nil nil 30 nil nil nil nil)
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "
      :data
      (-concat
       (let* ((lines (--map
                      (let* ((wines (brb-event-wines it)))
                        (list (buttonize
                               (or (brb-event-date-string it)
                                   "-")
                               set-date (vulpea-note-id it))
                              (buttonize "[P]" #'brb-event-plan it)
                              (concat
                               (if (vulpea-note-tagged-any-p it "external") "E" "I")
                               (if (string-equal "true" (vulpea-note-meta-get it "publish")) "+" "-"))
                              it
                              (buttonize
                               (if-let ((location (vulpea-note-meta-get it "location" 'note)))
                                   (vulpea-note-title location)
                                 "<unknown>")
                               set-location (vulpea-note-id it))
                              (buttonize
                               (if-let ((host (vulpea-note-meta-get it "host" 'note)))
                                   (or (vulpea-note-meta-get host "public name") (vulpea-note-title host))
                                 "<unknown>")
                               set-host (vulpea-note-id it))
                              (seq-length (brb-event-participants it))
                              (seq-length wines)
                              (brb-price-format (vulpea-meta-get it "price" 'number))))
                      (events-data-events x)))
              (today (format-time-string "%F" (current-time)))
              (splitted (--split-with (string< (nth 0 it) today) lines)))
         (-concat (nth 0 splitted)
                  (when (nth 1 splitted) '(sep))
                  (nth 1 splitted)))
       '(sep)
       `((""
          ""
          ""
          ""
          ""
          ""
          ,(--reduce-from
            (+ acc (seq-length (brb-event-participants it)))
            0 (events-data-events x))
          ,(--reduce-from
            (+ acc (seq-length (brb-event-wines it)))
            0 (events-data-events x))
          ""))))
     "\n")))

;; * Stats

(cl-defmethod brb-events--tab-stats ((x events-data))
  "Render overview tab for X."
  (events-data-load-summary-maybe x)
  (events-data-load-statement-maybe x)

  (let* ((today (format-time-string "%F" (current-time)))
         (events-all (events-data-events x))
         (events-past (--filter (string> today (brb-event-date-string it)) events-all))
         (events-summary (events-data-summary x))

         (wines-all (->> events-all
                         (--map (brb-event-wines it))
                         (-flatten)))
         (wines-past (->> events-past
                          (--map (brb-event-wines it))
                          (-flatten)))
         (wines (-distinct wines-past))

         (grapes-all (->> wines-all
                          (--map (vulpea-note-meta-get-list it "grapes" 'note))
                          (-flatten)))
         (grapes-past (->> wines-past
                           (--map (vulpea-note-meta-get-list it "grapes" 'note))
                           (-flatten)))
         (grapes (->> grapes-all
                      (-map #'vulpea-note-id)
                      (vulpea-db-query-by-ids)
                      (--filter (null (vulpea-note-primary-title it)))))

         (producers-all (->> wines-all
                             (--map (vulpea-note-meta-get it "producer" 'note))
                             (-flatten)))
         (producers-past (->> wines-past
                              (--map (vulpea-note-meta-get it "producer" 'note))
                              (-flatten)))
         (producers (->> producers-all
                         (-map #'vulpea-note-id)
                         (vulpea-db-query-by-ids)
                         (--filter (null (vulpea-note-primary-title it)))))

         (roas-all (->> wines-all
                        (--map (or (vulpea-note-meta-get it "region" 'note)
                                   (vulpea-note-meta-get it "appellation" 'note)))
                        (-flatten)))
         (roas-past (->> wines-past
                         (--map (or (vulpea-note-meta-get it "region" 'note)
                                    (vulpea-note-meta-get it "appellation" 'note)))
                         (-flatten)))
         (roas (->> roas-all
                    (-map #'vulpea-note-id)
                    (vulpea-db-query-by-ids)
                    (--filter (null (vulpea-note-primary-title it)))))

         (countries-all (->> wines-all
                             (--map (vulpea-note-meta-get
                                     (or (vulpea-note-meta-get it "region" 'note)
                                         (vulpea-note-meta-get it "appellation" 'note))
                                     "country" 'note))
                             (-flatten)))
         (countries-past (->> wines-past
                              (--map (vulpea-note-meta-get
                                      (or (vulpea-note-meta-get it "region" 'note)
                                          (vulpea-note-meta-get it "appellation" 'note))
                                      "country" 'note))
                              (-flatten)))
         (countries (->> countries-all
                         (-map #'vulpea-note-id)
                         (vulpea-db-query-by-ids)
                         (--filter (null (vulpea-note-primary-title it)))))

         (colours-all (->> wines-all
                           (--map (vulpea-note-meta-get it "colour" 'string))
                           (-flatten)))
         (colours-past (->> wines-past
                              (--map (vulpea-note-meta-get it "colour" 'string))
                              (-flatten)))
         (colours (->> colours-all
                       (-uniq))))

    ;; events overview
    (insert
     (propertize (format "Events (%d)" (seq-length events-all)) 'face 'outline-1)
     "\n\n"
     (string-table
      :header '("date" "event" "folks" "wines" "wavg" "price" "gain")
      :pad-type '(right right left left left left left)
      :width '(nil 30 nil nil nil nil nil nil)
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "
      :data
      (-concat
       (let* ((lines (--map
                      (let* ((wines (brb-event-wines it))
                             (summary (gethash (vulpea-note-id it) events-summary))
                             (date (brb-event-date-string it)))
                        (list (or date "-")
                              it
                              (seq-length (brb-event-participants it))
                              (seq-length wines)
                              (if-let ((wavg (assoc-default 'wavg summary)))
                                  (format "%.4f" wavg)
                                "-")
                              (brb-price-format (vulpea-meta-get it "price" 'number))
                              (if-let* ((statement (gethash (vulpea-note-id it) (events-data-statement x)))
                                        (gain (assoc-default 'balance-real statement)))
                                  (if (string> date today)
                                      "-"
                                    (brb-ledger--format-amount gain))
                                "-")))
                      events-all))
              (splitted (--split-with (string< (nth 0 it) today) lines)))
         (-concat (nth 0 splitted)
                  (when (nth 1 splitted) '(sep))
                  (nth 1 splitted)))
       '(sep)
       (list
        (list
         ""
         ""
         (--reduce-from
          (+ acc (seq-length (brb-event-participants it)))
          0 events-all)
         (--reduce-from
          (+ acc (seq-length (brb-event-wines it)))
          0 events-all)
         ""
         ""
         (->> events-past
              (--reduce-from
               (+ acc (assoc-default 'balance-real (gethash (vulpea-note-id it) (events-data-statement x))))
               0)
              (brb-price-format))))))
     "\n")

    (insert "\n")

    ;; locations
    (let* ((locs (->> events-all
                      (--map (vulpea-note-meta-get it "location" 'note))
                      (--map (if it (vulpea-note-title it) "unknown"))))
           (locs-uniq (-count-unique locs)))
      (insert
       (propertize (format "Locations (%d)" (seq-length locs-uniq)) 'face 'outline-1)
       "\n\n"
       (string-table
        :data (->> locs-uniq
                   (--sort (> (cdr it) (cdr other)))
                   (--map (list (car it) (cdr it))))
        :row-start "- "
        :pad-type '(right left)
        :sep "  "))
      (insert "\n"))

    (insert "\n")

    ;; participants
    (let* ((participants-past (->> events-past
                                   (--map (brb-event-participants it))
                                   (-flatten)))
           (participants-all (->> events-all
                                  (--map (brb-event-participants it))
                                  (-flatten)))
           (participants (-distinct participants-all)))
      (insert
       (propertize (format "Participants (%s)" (seq-length participants)) 'face 'outline-1)
       "\n\n"
       (string-table
        :header '("participant" "past" "all")
        :pad-type '(right left left)
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
              (list it
                    (-count (lambda (other)
                              (string-equal
                               (vulpea-note-id it)
                               (vulpea-note-id other)))
                            participants-past)
                    (-count (lambda (other)
                              (string-equal
                               (vulpea-note-id it)
                               (vulpea-note-id other)))
                            participants-all)))
             (--sort (> (nth 1 it)
                        (nth 1 other)))))
       "\n"))

    (insert "\n")

    ;; wines
    (insert
     (propertize (format "Wines (%s)" (seq-length wines)) 'face 'outline-1)
     "\n\n"
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
      (->> events-past
           (--map
            (let ((summary (alist-get 'wines (gethash (vulpea-note-id it) events-summary)))
                  (event it))
              (->> (brb-event-wines it)
                   (--map-indexed (list
                                   (brb-event-date-string event)
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
                                   (or (assoc-default 'price (nth it-index summary))
                                       "-")
                                   (if-let ((qpr (assoc-default 'qpr (nth it-index summary))))
                                       (format "%.4f" qpr) "-"))))))
           (-flatten-n 1)
           (--sort (string> (nth 5 it)
                            (nth 5 other)))))
     "\n")

    (insert "\n")

    ;; grapes
    (insert
     (propertize (format "Grapes (%s)" (seq-length grapes)) 'face 'outline-1)
     "\n\n"
     (string-table
      :header '("grape" "past" "all")
      :pad-type '(right left left)
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
            (list
             it
             (-count (lambda (other)
                       (string-equal
                        (vulpea-note-id it)
                        (vulpea-note-id other)))
                     grapes-past)
             (-count (lambda (other)
                       (string-equal
                        (vulpea-note-id it)
                        (vulpea-note-id other)))
                     grapes-all)))
           (--sort (> (nth 1 it)
                      (nth 1 other)))))
     "\n")

    (insert "\n")

    ;; countries
    (insert
     (propertize (format "Countries (%s)" (seq-length countries)) 'face 'outline-1)
     "\n\n"
     (string-table
      :header '("country" "past" "all")
      :pad-type '(right left left)
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
            (list
             it
             (-count (lambda (other)
                       (string-equal
                        (vulpea-note-id it)
                        (vulpea-note-id other)))
                     countries-past)
             (-count (lambda (other)
                       (string-equal
                        (vulpea-note-id it)
                        (vulpea-note-id other)))
                     countries-all)))
           (--sort (> (nth 1 it)
                      (nth 1 other)))))
     "\n")

    (insert "\n")

    ;; regions
    (insert
     (propertize (format "Regions (%s)" (seq-length roas)) 'face 'outline-1)
     "\n\n"
     (string-table
      :header '("region" "past" "all")
      :pad-type '(right left left)
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
            (list
             it
             (-count (lambda (other)
                       (string-equal
                        (vulpea-note-id it)
                        (vulpea-note-id other)))
                     roas-past)
             (-count (lambda (other)
                       (string-equal
                        (vulpea-note-id it)
                        (vulpea-note-id other)))
                     roas-all)))
           (--sort (> (nth 1 it)
                      (nth 1 other)))))
     "\n")

    (insert "\n")

    ;; colours
    (insert
     (propertize (format "Colours (%s)" (seq-length colours)) 'face 'outline-1)
     "\n\n"
     (string-table
      :header '("colour" "past" "all")
      :pad-type '(right left left)
      :header-sep "-"
      :header-sep-start "|-"
      :header-sep-conj "-+-"
      :header-sep-end "-|"
      :row-start "| "
      :row-end " |"
      :sep " | "
      :data
      (->> colours
           (--map
            (list
             it
             (-count (lambda (other)
                       (string-equal it other))
                     colours-past)
             (-count (lambda (other)
                       (string-equal it other))
                     colours-all)))
           (--sort (> (nth 1 it)
                      (nth 1 other)))))
     "\n")

    (insert "\n")))

(provide 'lib-brb-events)
;;; lib-brb-events.el ends here
