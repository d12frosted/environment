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

(defun vino-entry-find-file-available ()
  "Select and visit available `vino-entry'."
  (interactive)
  (let* ((available (seq-map
                     #'car
                     (inventory-balance-list
                      wine-inventory-file)))
         (res (vulpea-select
               "Wine"
               nil nil
               (lambda (note)
                 (let ((tags (vulpea-note-tags note)))
                   (and (seq-contains-p tags "wine")
                        (seq-contains-p tags "cellar")
                        (seq-contains-p available (vulpea-note-id note))))))))
    (if (vulpea-note-id res)
        (find-file (vulpea-note-path res))
      (user-error
       "Can not visit vino entry that does not exist: %s"
       (vulpea-note-title res)))))

(provide '+org-wine)
;;; +org-wine.el ends here
