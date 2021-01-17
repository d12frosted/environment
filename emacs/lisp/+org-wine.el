;;; +org-wine.el --- Wine tracking tool -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Nov 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require '+org)
(require '+org-pretty-props)
(require '+org-prop)
(require '+org-places)
(require '+org-buffer-prop)

;;
;; Ratings

(defvar wine--rating-props-v2
  '(("AROMA_QUALITY" . 3)
    ("AROMA_INTENSITY" . 2)
    ("AROMA_COMPLEXITY" . 3)
    ("BALANCE" . 3)
    ("FLAVOURS" . 2)
    ("AFTERTASTE" . 3)
    ("GENERAL" . 4))
  "Wine rating properties and their max value.")

(defvar wine--rating-props-v3
  '(("AROMA_QUALITY" .
     (lambda ()
       (let* ((total 3)
              (res total)
              (ans t)
              (quit-on "no taints")
              (opts (list
                     quit-on
                     "aggressive ethanol"
                     "massive brett attack"
                     "VA, especially nail polish removal")))
         (while ans
           (setq ans (completing-read "Any taints? " opts))
           (setq opts (delete ans opts))
           (if (string-equal ans "no taints")
               (setq ans nil)
             (setq res (max 0 (- res 1))))
           (when (equal res 0)
             (setq ans nil)))
         (cons res total))))

    ("AROMA_INTENSITY" .
     (("aroma can be perceived without putting nose into glass" . 2)
      ("aroma can be perceived only by putting nose into glass" . 1)
      ("closed, you need to put a lot of effort to get the aroma" . 0)))

    ("AROMA_RICHNESS" .
     (("more than 3 different notes" . 3)
      ("only 3 notes" . 2)
      ("only 2 notes" . 1)
      ("only 1 note" . 0)))

    ("AROMA_COMPLEXITY" .
     (("sophisticated, multilayered" . 1)
      ("simple" . 0)))

    ("BALANCE" .
     (("perfectly balanced, everything is in its place" . 3)
      ("well balanced, might be a small issue" . 2)
      ("average, either one bigger issue or two small" . 1)
      ("unbalanced, everything else" . 0)))

    ("FLAVOURS" .
     (("multiple flavours" . 1)
      ("only one flavour" . 0)))

    ("EVOLUTION" .
     (("taste and flavours evolve over time in mouth" . 1)
      ("plain, straightforward" . 0)))

    ("AFTERTASTE" .
     (("long, lasting more than 30 seconds" . 2)
      ("average, lasting more than 10 seconds" . 1)
      ("short" . 0)))

    ("GENERAL" .
     (("life changing" . 4)
      ("great wine, I will definitely look into tasting it once more" . 3)
      ("good wine, will drink it again with pleasure if situation arises" . 2)
      ("average wine, only with parents" . 1)
      ("bad wine, only for enemies" . 0))))
  "Wine rating properties and possible values.")

;;
;; Migration

(defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
	(throw 'nextfile t))

(defun wine/migrate-wine ()
  "Migrate producer to note."
  (interactive)
  (let* ((p (point))
         (old-id (org-entry-get p "ID"))
         (colour (pcase (org-entry-get p "COLOUR")
                   (`"White" 'white)
                   (`"Red" 'red)
                   (`"Rose" 'rose)
                   (`"Orange" 'white)
                   (v (user-error "Unknown colour %s" v))))
         (carbonation (pcase (org-entry-get p "CARBONATION")
                        (`"Still" 'still)
                        (`"Semi-sparkling" 'sparkling)
                        (`"Sparkling" 'sparkling)
                        (v (user-error "Unknown carbonation %s" v))))
         (sweetness (pcase (org-entry-get p "SWEETNESS")
                      (`"Dry" 'dry)
                      (`"Semi-dry" 'semy-dry)
                      (`"Semi-sweet" 'semy-sweet)
                      (`"Sweet" 'sweet)
                      (`"Brut" 'brut)
                      (`"Brut Nature" 'brut-nature)
                      (`"Extra Dry" 'extra-dry)
                      (`"Extra Brut" 'extra-brut)
                      (v (user-error "Unknown sweetness %s" v))))
         (winery-id (+org-extract-id-from-link (org-entry-get p "WINERY")))
         (name (org-entry-get p "NAME"))
         (vintage (org-entry-get p "YEAR"))
         (appellation (+org-extract-id-from-link (org-entry-get p "APPELLATION")))
         (grapes (seq-map (lambda (x)
                            (+org-extract-id-from-link
                             (s-trim-left (concat x "]]"))))
                          (seq-reverse
                           (cdr
                            (seq-reverse
                             (+org-entry-get-list "GRAPES" "\\]\\]"))))))
         (price (+org-entry-get-list "PRICE" ", "))
         (sugar (let ((v (+org-entry-get-number "SUGAR")))
                  (if (< v 0)
                      nil
                    v)))
         (alcohol (+org-entry-get-number "ALCOHOL"))
         (volume (+org-entry-get "VOLUME"))
         (resources (org-drawer-list "RESOURCES"))
         (ratings (org-map-entries
                   (lambda ()
                     (let* ((version (or (+org-entry-get-number "RATING_VERSION")
                                         1))
                            (props (pcase version
                                     (`3 wine--rating-props-v3)
                                     (`2 wine--rating-props-v2)
                                     (`1 '(("SCORE" . 10)))
                                     (v (user-error "Unsupported rating version %i" version))))
                            (date (+org-entry-get "DATE"))
                            (values (seq-map
                                     (lambda (cfg)
                                       (if (= version 1)
                                           (list "SCORE"
                                                 (+org-entry-get-number "TOTAL")
                                                 10)
                                         (list (car cfg)
                                               (+org-entry-get-number (car cfg))
                                               (+org-entry-get-number (concat (car cfg) "_MAX")))))
                                     props))
                            (subtree (org-copy-subtree))
                            (content (with-temp-buffer
                                       (org-mode)
                                       (org-paste-subtree)
                                       (let* ((els (org-element-context))
                                              (p1 (org-element-property :contents-begin els))
                                              (p2 (org-element-property :contents-end els)))
                                         (goto-char p1)
                                         (setq p1 (org-element-property :end (org-element-context)))
                                         (buffer-substring p1 p2)))))
                       (list :date date
                             :values values
                             :version version
                             :content content)))
                   "RATING"
                   'tree))
         (headings-to-move (cdr
                            (org-map-entries
                             (lambda ()
                               (org-copy-subtree)
                               (with-temp-buffer
                                 (org-mode)
                                 (org-paste-subtree)
                                 (goto-char (point-min))
                                 (ignore-errors (org-metaleft))
                                 (buffer-substring (point-min) (point-max))))
                             "-RATING"
                             'tree)))
         (wine-content (progn
                         (org-copy-subtree)
                         (with-temp-buffer
                           (org-mode)
                           (org-paste-subtree)
                           (widen)
                           (let* ((els (org-element-context))
                                  (p1 (org-element-property :contents-begin els))
                                  (p2 (org-element-property :contents-end els)))
                             (goto-char p1)
                             (setq p1 (org-element-property :end (org-element-context)))
                             (goto-char p1)
                             (let* ((ctx (org-element-context)))
                               (when (equal 'drawer (org-element-type ctx))
                                 (setq p1 (org-element-property :end (org-element-context)))
                                 (goto-char p1)))
                             (let* ((ctx (org-element-context)))
                               (unless (equal 'headline (org-element-type ctx))
                                 (org-next-visible-heading 1)))
                             (setq p2 (org-element-property :begin (org-element-context)))
                             (buffer-substring-no-properties p1 p2))))))

    ;; now we have all the information, so let's create a vino entry
    (let* ((vino-entry (make-vino-entry
                        :carbonation carbonation
                        :colour colour
                        :sweetness sweetness
                        :producer winery-id
                        :name name
                        :vintage vintage
                        :appellation appellation
                        :region nil
                        :grapes grapes
                        :alcohol alcohol
                        :sugar sugar
                        :resources resources
                        :price price
                        :acquired 0
                        :consumed 0
                        :rating nil
                        :ratings nil))
           (vino-id (vino-entry--create vino-entry old-id))
           (_ (org-roam-db-build-cache))
           (vino-note (vino-entry-note-get-dwim vino-id)))
      ;; we now have a file dedicated for vino entry
      ;; let's fill it with headings-to-move and wine-content
      (vulpea-utils-with-file (vulpea-note-path vino-note)
        (goto-char (point-max))
        (insert "\n")
        (unless (string-empty-p wine-content)
          (insert wine-content)
          (insert "\n"))
        (seq-do (lambda (str)
                  (insert str "\n"))
                headings-to-move))

      ;; and don't forget to update availability
      (vino-entry-update-availability vino-id)

      ;; now, let's rate
      (seq-do (lambda (rcfg)
                (let ((rid (vino-entry-rate--create
                            vino-id
                            (plist-get rcfg :date)
                            (plist-get rcfg :version)
                            (plist-get rcfg :values))))
                  (vulpea-utils-with-file (vulpea-note-path (vulpea-db-get-by-id rid))
                    (goto-char (point-max))
                    (insert "\n")
                    (insert (plist-get rcfg :content))
                    (insert "\n"))))
              ratings)
      (org-cut-subtree))))

(provide '+org-wine)
;;; +org-wine.el ends here
