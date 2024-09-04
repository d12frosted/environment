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
       (--filter (let ((date (brb-event-date-string it)))
                   (and (org-time>= date (nth 0 range))
                        (org-time< date (nth 1 range)))))
       (--sort (org-time< (brb-event-date-string it)
                          (brb-event-date-string other)))))

;;;###autoload
(defun brb-events-without-date ()
  "Return list of events without any date set."
  (->> (vulpea-db-query-by-tags-every '("wine" "event" "barberry/public"))
       (--filter (= 0 (vulpea-note-level it)))
       (-remove #'brb-event-date-string)))

;; * Metadata

;;;###autoload
(defun brb-event-date-string (event)
  "Return date of EVENT as string using %F format."
  (vulpea-utils-with-note event
    (when-let ((str (vulpea-buffer-prop-get "date")))
      (org-read-date nil nil str))))

;; * Event creation

;;;###autoload
(defun brb-event-create ()
  "Create a new event."
  (interactive)
  (let* ((name (read-string "Name: "))
         (slug (read-string "Slug: "))
         (date-str
          (with-temp-buffer
            (let ((date (org-read-date nil t nil "Date: ")))
              (org-insert-timestamp date nil t)
              (buffer-substring (point-min) (point-max))))))
    (vulpea-create
     name
     "wine/event/%<%Y%m%d%H%M%S>-${slug}.org"
     :tags '("wine" "event" "barberry/public" "barberry/post")
     :head (->> (list (cons "date" date-str)
                      (cons "slug" slug)
                      (cons "tags" "report")
                      (cons "language" "EN")
                      (cons "author" "Boris")
                      (cons "description" "")
                      (cons "image" ""))
                (--map (format "#+%s: %s" (car it) (cdr it)))
                (s-join "\n"))
     :body "- publish :: false\n- time to book :: N/A\n"
     :immediate-finish t)))

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
             (price-asking . num)
             (participants . (id))
             (type . str)
             (ignore-scores . bool)
             (volume . num)
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

;;;###autoload
(cl-defun brb-event-statement (event &key data participants wines host balances)
  "Prepare statement for EVENT.

DATA is loaded unless provided.
HOST is loaded unless provided.
PARTICIPANTS are loaded unless provided.
WINES are loaded unless provided.
BALANCES is a hash table."
  (let* ((data (or data (brb-event-data-read event)))
         (participants (or participants (brb-event-participants event)))
         (wines (or wines (brb-event-wines event)))
         (host (or host (vulpea-note-meta-get event "host" 'note)))

         ;; calculations
         (price (vulpea-note-meta-get event "price" 'number))
         (wines-normal (->> data
                            (assoc-default 'wines)
                            (--filter (string-equal "normal" (assoc-default 'type it)))))
         (wines-extra (->> data
                           (assoc-default 'wines)
                           (--filter (string-equal "extra" (assoc-default 'type it)))))

         (spending-shared (->> data
                               (assoc-default 'shared)
                               (--map (ceiling
                                       (* (assoc-default 'amount it)
                                          (assoc-default 'price it))))
                               (-sum)))
         (spending-order (->> data
                              (alist-get 'personal)
                              (--map
                               (->> (alist-get 'orders it)
                                    (-map (lambda (od)
                                            (let ((amount (if (string-equal brb-event-narrator-id
                                                                            (alist-get 'participant od))
                                                              0
                                                            (alist-get 'amount od))))
                                              (* amount (alist-get 'price it)))))
                                    (-sum)))
                              (-sum)))
         (spending-wines-public (->> wines-normal
                                     (--map (assoc-default 'price-public it))
                                     (--filter it)
                                     (-sum)))
         (spending-wines-real (->> wines-normal
                                   (--map (assoc-default 'price-real it))
                                   (--filter it)
                                   (-sum)))
         (spending-extra-public (->> wines-extra
                                     (--map (assoc-default 'price-public it))
                                     (--filter it)
                                     (-sum)))
         (spending-extra-real (->> wines-extra
                                   (--map (assoc-default 'price-real it))
                                   (--filter it)
                                   (-sum)))
         (credit-public (+ spending-wines-public spending-extra-public spending-shared))
         (credit-real (+ spending-wines-real spending-extra-real spending-shared))
         (debit-base (->> participants
                          (--map (alist-get 'fee (brb-event-statement-for event it
                                                                          :data data
                                                                          :host host
                                                                          :wines wines
                                                                          :balances balances)))
                          (-sum)))
         (debit-extra (->> wines-extra
                           (--map
                            (let* ((ps (-remove-item brb-event-narrator-id (assoc-default 'participants it)))
                                   (glass-price (ep--glass-price it)))
                              (* glass-price (length ps))))
                           (-sum)))
         (debit (+ debit-base debit-extra))
         (balance-public (- debit credit-public))
         (balance-real (- debit credit-real)))
    `((spending-shared . ,spending-shared)
      (spending-order . ,spending-order)
      (spending-wines-public . ,spending-wines-public)
      (spending-wines-real . ,spending-wines-real)
      (spending-extra-public . ,spending-extra-public)
      (spending-extra-real . ,spending-extra-real)
      (credit-public . ,credit-public)
      (credit-real . ,credit-real)
      (debit-base . ,debit-base)
      (debit-extra . ,debit-extra)
      (debit . ,debit)
      (balance-public . ,balance-public)
      (balance-real . ,balance-real))))

;;;###autoload
(cl-defun brb-event-empty-statement-for (event participant &key data wines balances)
  "Prepare statement for PARTICIPANT of EVENT.

DATA is loaded unless provided.
WINES is a list of `vulpea-note'. Loaded unless provided.
BALANCES is a hash table."
  (let* ((data (or data (brb-event-data-read event)))
         (use-balance (pcase (or (vulpea-note-meta-get event "use balance") "true")
                        ("true" t)
                        (_ nil)))
         (wines (or wines (brb-event-wines event)))
         (pid (vulpea-note-id participant))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (fee 0)
         (mode "normal")
         (order nil)
         (extra nil)
         (balance (if use-balance
                      (or (gethash pid balances) 0)
                    0))
         (total 0)
         (due 0)
         (balance-final balance))
    `((balance . ,balance)
      (balance-final . ,balance-final)
      (mode . ,mode)
      (fee . ,fee)
      (order . ,order)
      (extra . ,extra)
      (total . ,total)
      (due . ,due))))

;;;###autoload
(cl-defun brb-event-statement-for (event participant &key data host wines balances)
  "Empty statement for PARTICIPANT of EVENT.

DATA is loaded unless provided.
HOST is a `vulpea-note'. Loaded unless provided.
WINES is a list of `vulpea-note'. Loaded unless provided.
BALANCES is a hash table."
  (let* ((data (or data (brb-event-data-read event)))
         (use-balance (pcase (or (vulpea-note-meta-get event "use balance") "true")
                        ("true" t)
                        (_ nil)))
         (host (or host (vulpea-note-meta-get event "host" 'note)))
         (wines (or wines (brb-event-wines event)))
         (host-id (when host (vulpea-note-id host)))
         (pid (vulpea-note-id participant))
         (price (or (vulpea-note-meta-get event "price" 'number) 0))
         (host-p (string-equal pid host-id))
         (fee (if host-p
                  0
                (or (->> data
                         (alist-get 'participants)
                         (--find (string-equal pid (alist-get 'id it)))
                         (alist-get 'fee))
                    price)))
         (mode (cond
                (host-p "host")
                ((/= fee price) "custom")
                (t "normal")))
         (order (->> data
                     (alist-get 'personal)
                     (--map
                      (let* ((od (->> (alist-get 'orders it)
                                      (--find (string-equal pid (alist-get 'participant it)))))
                             (amount (or (when od (alist-get 'amount od))
                                         0)))
                        `((item . ,(alist-get 'item it))
                          (price . ,(alist-get 'price it))
                          (amount . ,amount)
                          (total . ,(* amount (alist-get 'price it))))))
                     (--filter (> (alist-get 'amount it) 0))))
         (extra (->> data
                     (alist-get 'wines)
                     (--filter (string-equal "extra" (assoc-default 'type it)))
                     (--filter (-contains-p (assoc-default 'participants it) pid))
                     (--map
                      (let* ((ps (alist-get 'participants it))
                             (wid (alist-get 'id it))
                             (price (or (alist-get 'price-asking it)
                                        (alist-get 'price-public it)))
                             (glass-price (ceiling (/ price (float (length ps)))))
                             (wine (--find (string-equal wid (vulpea-note-id it)) wines)))
                        `((glass-price . ,glass-price)
                          (amount . 1)
                          (total . ,glass-price)
                          (wine . ,wine))))
                     (--filter (alist-get 'wine it))))
         (balance (if use-balance
                      (or (gethash pid balances) 0)
                    0))
         (total (+ fee
                   (-sum (--map (alist-get 'total it) order))
                   (-sum (--map (alist-get 'glass-price it) extra))))
         (due (max 0 (- total balance)))
         (balance-final (- balance total)))
    `((balance . ,balance)
      (balance-final . ,balance-final)
      (mode . ,mode)
      (fee . ,fee)
      (order . ,order)
      (extra . ,extra)
      (total . ,total)
      (due . ,due))))

;;;###autoload
(defun brb-event-statement-add (s1 s2)
  "Add two statements S1 and S2."
  (let ((balance (+ (alist-get 'balance s1)
                    (alist-get 'balance s2)))
        (balance-final (+ (alist-get 'balance-final s1)
                          (alist-get 'balance-final s2)))
        (mode (alist-get 'mode s1))
        (fee (+ (alist-get 'fee s1)
                (alist-get 'fee s2)))
        (order (let* ((o1 (alist-get 'order s1))
                      (o2 (alist-get 'order s2))
                      (items (-uniq (append
                                     (--map (alist-get 'item it) o1)
                                     (--map (alist-get 'item it) o2)))))
                 (--map
                  (let* ((item it)
                         (i1 (--find (string-equal item (alist-get 'item it)) o1))
                         (i2 (--find (string-equal item (alist-get 'item it)) o2))
                         (price (alist-get 'price (or i1 i2)))
                         (amount (+ (or (alist-get 'amount i1) 0)
                                    (or (alist-get 'amount i2) 0)))
                         (total (+ (or (alist-get 'total i1) 0)
                                   (or (alist-get 'total i2) 0))))
                    `((item . ,item)
                      (price . ,price)
                      (amount . ,amount)
                      (total . ,total)))
                  items)))
        (extra (--reduce-from
                (let* ((extra it)
                       (data (--find
                              (string-equal (vulpea-note-id (alist-get 'wine it))
                                            (vulpea-note-id (alist-get 'wine extra)))
                              acc))
                       (glass-price (alist-get 'glass-price extra))
                       (amount (+ (alist-get 'amount extra) (or (alist-get 'amount data) 0)))
                       (total (+ (alist-get 'total extra) (or (alist-get 'total data) 0)))
                       (wine (alist-get 'wine extra)))
                  (-snoc
                   (--remove (string-equal (vulpea-note-id (alist-get 'wine it))
                                           (vulpea-note-id (alist-get 'wine extra)))
                             acc)
                   `((glass-price . ,glass-price)
                     (amount . ,amount)
                     (total . ,total)
                     (wine . ,wine))))
                nil
                (append (alist-get 'extra s1)
                        (alist-get 'extra s2))))
        (total (+ (alist-get 'total s1)
                  (alist-get 'total s2)))
        (due (+ (alist-get 'due s1)
                (alist-get 'due s2))))
    `((balance . ,balance)
      (balance-final . ,balance-final)
      (mode . ,mode)
      (fee . ,fee)
      (order . ,order)
      (extra . ,extra)
      (total . ,total)
      (due . ,due))))

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
                                                    (error "Could not find weight data for participant %s for '%s' event" pid (vulpea-note-title event)))
                                                  (calcFunc-mul (assoc-default 'weight weight-data)
                                                                (assoc-default 'score it))))
                                               (apply #'calcFunc-vec)
                                               (calcFunc-vsum)))
                                  (wavg (when scores (/ wtotal weights-sum)))
                                  (price (alist-get 'price-public data))
                                  (volume (or (alist-get 'volume data) 750))
                                  (qpr (brb-qpr price wavg volume))
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
                                                     (sentiment . ,(alist-get 'sentiment it)))))))
                                  (pscores (->>
                                            participants
                                            (--map
                                             (let* ((pid (vulpea-note-id it))
                                                    (sd (--find
                                                         (string-equal pid (alist-get 'participant it))
                                                         (alist-get 'scores data))))
                                               `((participant . ,it)
                                                 (score . ,(alist-get 'score sd))
                                                 (sentiment . ,(alist-get 'sentiment sd))))))))
                             `((wine . ,wine)
                               (ignore-scores . ,(alist-get 'ignore-scores data))
                               (type . ,(alist-get 'type data))
                               (volume . ,volume)
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
                      (--filter it)
                      (--remove (= 0 it))))
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
         (rms-scores (->> wines-data
                         (--remove (alist-get 'ignore-scores it))
                         (--map (alist-get 'rms it))
                         (--filter it)
                         (-map #'calc-from-number)))
         (wavg-scores (->> wines-data
                         (--remove (alist-get 'ignore-scores it))
                         (--map (alist-get 'wavg it))
                         (--filter it)
                         (-map #'calc-from-number)))
         (event-rms (when rms-scores
                      (->> rms-scores
                           (apply #'calcFunc-vec)
                           (calcFunc-rms)
                           (calc-to-number))))
         (event-wavg (when wavg-scores
                       (->> wavg-scores
                            (apply #'calcFunc-vec)
                            (calcFunc-rms)
                            (calc-to-number))))
         (event-qpr (when (and event-wavg prices) (brb-qpr wines-price-harmonic event-wavg))))
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
