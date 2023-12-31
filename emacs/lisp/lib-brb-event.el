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
  (vulpea-utils-with-note event
    (brb-event--participants)))

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

;; * Summary

;;;###autoload
(defun brb-event-summary (event)
  "Return score summary of EVENT."
  (let* ((tbl (brb-event-score-data event))
         (wines (brb-event-wines event))
         (summary (brb-event-score--calc-summary tbl))
         (amean (assoc-default 'amean summary))
         (rms (assoc-default 'rms summary))
         (wavg (assoc-default 'wavg summary))
         (sdevs (assoc-default 'sdevs summary))
         (favs (assoc-default 'favs summary))
         (outs (assoc-default 'outs summary))
         (prices (assoc-default 'prices summary))
         (qprs (assoc-default 'qprs summary))

         (people (->> tbl
                      (-map 'car)
                      (-remove 'string-empty-p)
                      (--map (if (string-match-p string-uuid-regexp it)
                                 (vulpea-db-get-by-id (string-match-1 string-uuid-regexp it))
                               it))))
         (ratings (-map #'identity (table-select-rows "rating" tbl :column 1)))
         (favourites (-map (-rpartial #'-positions-of '("favourite" "fav" "+"))
                           (table-select-rows "extremum" tbl :column 1)))
         (outcasts (-map (-rpartial #'-positions-of '("outcast" "out" "-"))
                         (table-select-rows "extremum" tbl :column 1)))

         (wines-summary (--map-indexed
                         `((wine . ,(nth it-index wines))
                           (amean . ,(nth it-index amean))
                           (rms . ,(nth it-index rms))
                           (wavg . ,(nth it-index wavg))
                           (sdev . ,(nth it-index sdevs))
                           (fav . ,(nth it-index favs))
                           (out . ,(nth it-index outs))
                           (price . ,(nth it-index prices))
                           (qpr . ,(nth it-index qprs))
                           (scores . ,(->>
                                       (-zip-pair people (--map (nth it-index it) ratings))
                                       (-map-indexed
                                        (lambda (p-index p)
                                          (list
                                           (cons 'convive (car p))
                                           (cons 'score (unless (string-empty-p (cdr p))
                                                         (string-to-number (cdr p))))
                                           (cons 'extremum
                                            (cond
                                             ((-contains-p (nth p-index favourites) (+ it-index 1)) 'fav)
                                             ((-contains-p (nth p-index outcasts) (+ it-index 1)) 'out)
                                             (t nil)))))))))
                         (assoc-default 'wines summary)))

         (prices (-filter #'identity prices))
         (wines-price-total (-sum prices))
         (wines-price-harmonic (when prices
                                 (->> prices
                                      (-filter #'identity)
                                      (-map #'calc-from-number)
                                      (apply #'calcFunc-vec)
                                      (calcFunc-vhmean)
                                      (calc-to-number))))
         (wines-price-median (when prices
                               (->> prices
                                    (-filter #'identity)
                                    (-map #'calc-from-number)
                                    (apply #'calcFunc-vec)
                                    (calcFunc-vmedian)
                                    (calc-to-number))))
         (event-rms (when prices
                      (->> wavg
                           (-filter #'identity)
                           (-map #'calc-from-number)
                           (apply #'calcFunc-vec)
                           (calcFunc-rms)
                           (calc-to-number))))
         (event-qpr (when prices (brb-qpr wines-price-harmonic event-rms))))
    `((wines . ,wines-summary)
      (wines-price-total . ,wines-price-total)
      (wines-price-harmonic . ,wines-price-harmonic)
      (wines-price-median . ,wines-price-median)
      (rms . ,event-rms)
      (qpr . ,event-qpr))))

;; * Scores

;;;###autoload
(defun brb-event-score-data (event)
  "Return raw score data (table) of EVENT."
  (vulpea-utils-with-note event
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "#\\+name: data")
      (beginning-of-line)
      (while (looking-at "#\\+")
        (forward-line))
      (->> (org-table-to-lisp)
           (-filter #'listp)
           (--map (--map (if (stringp it) (substring-no-properties it) it) it))))))

(cl-defun brb-event-raw-scores-to-summary (tbl &key columns)
  "Convert raw scores to summary.

TBL represents raw scores.

When COLUMNS is not specified, all columns are returned.
Otherwise only those specified in the list."
  (let* ((summary (brb-event-score--calc-summary tbl))
         (wines (assoc-default 'wines summary))
         (amean (assoc-default 'amean summary))
         (rms (assoc-default 'rms summary))
         (wavg (assoc-default 'wavg summary))
         (sdevs (assoc-default 'sdevs summary))
         (favs (assoc-default 'favs summary))
         (outs (assoc-default 'outs summary))
         (prices (assoc-default 'prices summary))
         (qprs (assoc-default 'qprs summary))
         (columns (or columns '("amean" "rms" "wavg" "sdev" "favourite" "outcast" "price" "QPR"))))
    (-concat
     (list (cons "" columns)
           'hline)
     (-map
      (lambda (i)
        (-filter
         #'identity
         (list
          (nth i wines)
          (when (-contains-p columns "amean")
            (brb-format-float-in (nth i amean) :floats amean :fn #'-max :style 'bold :prec 4))
          (when (-contains-p columns "rms")
            (brb-format-float-in (nth i rms) :floats rms :fn #'-max :style 'bold :prec 4))
          (when (-contains-p columns "wavg")
            (brb-format-float-in (nth i wavg) :floats wavg :fn #'-max :style 'bold :prec 4))
          (when (-contains-p columns "sdev")
            (brb-format-float-in (nth i sdevs) :prec 4))
          (when (-contains-p columns "favourite")
            (brb-format-float-in (nth i favs) :floats favs :fn #'-max :style 'bold))
          (when (-contains-p columns "outcast")
            (brb-format-float-in (nth i outs) :floats outs :fn #'-max :style 'del))
          (when (-contains-p columns "price")
            (brb-format-float-in (nth i prices)))
          (when (-contains-p columns "QPR")
            (brb-format-float-in (nth i qprs) :floats qprs :fn #'-max :style 'bold :prec 4)))))
      (-iota (seq-length wines))))))

(defun brb-event-score--calc-summary (tbl)
  "Calculate summary from TBL.

TBL is a data table of the following format:

|               |            | Wine | Wine | ... | Wine |
|---------------+------------+------+------+-----+------|
|               | prop 1     | val  | val  |     | val  |
|               | prop 2     | val  | val  |     | val  |
|               | ...        |      |      |     |      |
|               | prop 3     | val  | val  |     | val  |
|---------------+------------+------+------+-----+------|
| Participant 1 |            |      |      |     |      |
|               | rating     | num  | num  |     | num  |
|               | extremum   | fav  |      |     | out  |
|               | ...        |      |      |     |      |
|               | extra prop | val  | val  |     | val  |
|---------------+------------+------+------+-----+------|
| Participant 2 |            |      |      |     |      |
|               | rating     | num  | num  |     | num  |
|               | extremum   | fav  |      |     | out  |
|               | ...        |      |      |     |      |
|               | extra prop | val  | val  |     | val  |
|---------------+------------+------+------+-----+------|
| ...           |            |      |      |     |      |
|               | rating     |      |      |     |      |
|               | extremum   |      |      |     |      |
|               | ...        |      |      |     |      |
|               | extra prop |      |      |     |      |
|---------------+------------+------+------+-----+------|
| Participant N |            |      |      |     |      |
|               | rating     | num  | num  |     | num  |
|               | extremum   | fav  |      |     | out  |
|               | ...        |      |      |     |      |
|               | extra prop | val  | val  |     | val  |
|---------------+------------+------+------+-----+------|

Participant can be a link to `vulpea-note'."
  (let* ((count (- (length (car tbl)) 2))
         (names (-drop 2 (car tbl)))

         ;; extract weights
         (people (->> tbl (-map 'car) (-remove 'string-empty-p)))
         (weight-def 2)
         (weights (->> people
                       (--map (if (s-matches? string-uuid-regexp it)
                                  (or (vulpea-note-meta-get
                                       (vulpea-db-get-by-id (string-match-1 string-uuid-regexp it))
                                       "tasting level"
                                       'number)
                                      weight-def)
                                weight-def))
                       (--map (* it it))))

         (ratings (->> (table-select-rows "rating" tbl :column 1)
                       (--map (--map (cond
                                      ((numberp it) it)
                                      ((and (stringp it) (string-non-empty-p it)) (string-to-number it))
                                      (t nil))
                                     it))))

         (prices (->> (car (table-select-rows "price" tbl :column 1))
                      (--map (cond
                              ((numberp it) it)
                              ((and (stringp it) (string-non-empty-p it)) (string-to-number it))
                              (t nil)))))

         (totals (table-vreduce-columns #'calcFunc-vsum ratings))
         (amean (table-vreduce-columns #'calcFunc-vmean ratings))
         (rms (table-vreduce-columns (lambda (&rest vecs) (calcFunc-rms (apply #'calcFunc-vec vecs))) ratings))

         ;; this is a bit hard to read, but we need to support cases
         ;; when someone DOESN'T rate a wine.
         (wavg (->> (table-transpose ratings)
                    (--map (-zip-with
                            (lambda (a b)
                              (when a
                                (cons (calc-from-number a) (calc-from-number b))))
                            it
                            weights))
                    (--map (-filter #'identity it))
                    (--map (when it
                             (calc-to-number
                              (calcFunc-div
                               (calcFunc-vsum (apply #'calcFunc-vec
                                                     (--map (calcFunc-mul (car it) (cdr it)) it)))
                               (calcFunc-vsum (apply #'calcFunc-vec (-map #'cdr it)))))))))
         (sdevs (table-vreduce-columns #'calcFunc-vpvar ratings))
         (favourites (-filter #'identity
                              (-map (-rpartial #'-positions-of '("favourite" "fav" "+"))
                                    (table-select-rows "extremum" tbl :column 1))))
         (outcasts (-filter #'identity
                            (-map (-rpartial #'-positions-of '("outcast" "out" "-"))
                                  (table-select-rows "extremum" tbl :column 1))))
         (favourited (-map (lambda (i) (-count (-rpartial #'-contains-p i) favourites))
                           (-iota count 1)))
         (outcasted (-map (lambda (i) (-count (-rpartial #'-contains-p i) outcasts))
                          (-iota count 1)))

         (qprs (-map (lambda (i) (brb-qpr (nth i prices) (nth i rms))) (-iota count))))
    `((wines . ,names)
      (ratings . ,ratings)
      (totals . ,totals)
      (amean . ,amean)
      (rms . ,rms)
      (wavg . ,wavg)
      (sdevs . ,sdevs)
      (prices . ,prices)
      (qprs . ,qprs)
      (favs . ,favourited)
      (outs . ,outcasted))))

(defun brb-event-raw-scores-to-people (tbl)
  "Convert raw scores to individual scores.

TBL represents raw scores."
  (let* ((wines (-drop 2 (car tbl)))
         (people (->> tbl (-map 'car) (-remove 'string-empty-p)))
         (ratings (-filter #'identity (-map #'identity (table-select-rows "rating" tbl :column 1))))
         (favourites (-map (-rpartial #'-positions-of '("favourite" "fav" "+"))
                           (table-select-rows "extremum" tbl :column 1)))
         (outcasts (-map (-rpartial #'-positions-of '("outcast" "out" "-"))
                         (table-select-rows "extremum" tbl :column 1))))
    (-concat
     (list
      (cons " " wines)
      'hline)
     (-map-indexed
      (lambda (i p)
        (let ((rs (nth i ratings))
              (convive (if (string-match-p string-uuid-regexp p)
                           (let ((note (vulpea-db-get-by-id (string-match-1 string-uuid-regexp p))))
                             (or (vulpea-note-meta-get note "public name")
                                 (user-error "%s doesn't have public name, please set it up"
                                             (vulpea-note-title note))))
                         p)))
          (cons
           convive
           (-map-indexed
            (lambda (ri r)
              (brb-format-float r
                :style
                (cond
                 ((-contains-p (nth i favourites) (1+ ri)) 'bold)
                 ((-contains-p (nth i outcasts) (1+ ri)) 'del)
                 (t 'normal))))
            rs))))
      people))))

;;;###autoload
(defun brb-event-score-personal (event)
  "Return personal scores of EVENT."
  (let* ((tbl (brb-event-score-data event))
         (people (->> tbl (-map 'car) (-remove 'string-empty-p) (-map #'substring-no-properties)))
         (ratings (-filter #'identity (-map #'identity (table-select-rows "rating" tbl :column 1))))
         (favourites (-map (-rpartial #'-positions-of '("favourite" "fav" "+"))
                           (table-select-rows "extremum" tbl :column 1)))
         (outcasts (-map (-rpartial #'-positions-of '("outcast" "out" "-"))
                         (table-select-rows "extremum" tbl :column 1))))
    (--map-indexed
     (let ((rs (nth it-index ratings)))
       (list
        :convive
        (cond
         ((string-match string-uuid-regexp it) (match-string 1 it))
         (t it))
        :ratings
        (--map
         (cond
          ((and (stringp it) (string-empty-p it)) nil)
          ((stringp it) (string-to-number it))
          (t it))
         rs)
        :favourites (nth it-index favourites)
        :outcasts (nth it-index outcasts)))
     people)))

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

;;;###autoload
(defun brb-event-add-participant ()
  "Select and add a participant to event."
  (interactive)
  (let ((note (vulpea-select-from
               "Participant"
               (vulpea-db-query-by-tags-every '("people"))
               :require-match t)))
    ;; insert to the list of participants
    (brb-event-add-participant--to-list note)
    ;; add to the table
    (brb-event-add-participant--to-data note)))

(defun brb-event-add-participant--to-list (participant)
  "Add PARTICIPANT to the currently visited event.

Noop in case the participant is already part of the list."
  (unless (--find
           (string-equal (vulpea-note-id it)
                         (vulpea-note-id participant))
           (brb-event--participants))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (format org-complex-heading-regexp-format (regexp-quote "Preparation")))
      (search-forward "1. ")
      (beginning-of-line)
      (goto-char (org-element-property :contents-end (org-element-at-point)))
      (insert "1. "
              (org-link-make-string
               (concat "id:" (vulpea-note-id participant))
               (vulpea-note-title participant)))
      (beginning-of-line)
      (funcall-interactively #'org-ctrl-c-ctrl-c)
      (end-of-line)
      (insert "\n"))))

(defun brb-event-add-participant--to-data (participant)
  "Add PARTICIPANT to the currently visited event.

Noop in case the participant is already part of the list."
  (save-excursion
    (goto-char (point-min))
    (search-forward "#+name: data")
    (let ((end (org-element-property :contents-end (org-element-at-point))))
      (unless (save-excursion
                (search-forward (vulpea-note-id participant) end 'no-error))
        (goto-char end)
        (forward-line -1)
        (let ((end (point)))
          (search-backward "|-")
          (copy-region-as-kill (point) end)
          (yank))
        (search-backward "|-")
        (forward-line 1)
        (let ((x (+ (point) 2)))
          (goto-char x)
          (search-forward "|")
          (delete-region x (- (point) 2))
          (goto-char x)
          (insert (org-link-make-string
                   (concat "id:" (vulpea-note-id participant))
                   (vulpea-note-meta-get participant "public name")))
          (funcall-interactively #'org-ctrl-c-ctrl-c))))))

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
