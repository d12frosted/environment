;;; init-vino.el --- Vino support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 13 Feb 2021
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
;; This module adds support of `vino' package.
;;
;;; Code:

(use-package vino
  :elpaca (vino :host github
                :repo "d12frosted/vino"
                :branch "v0.4.0")
  :defer t
  :commands (vino-setup)
  :general
  (leader-def
    "v" '(nil :which-key "vino...")
    "vl" '(nil :which-key "ledger...")
    "vlo" '(brb-ledger-display :which "display")
    "vld" '(brb-ledger-display :which "deposit")
    "vls" '(brb-ledger-display :which "spend")
    "vv" '(vino-entry-find-file :which-key "find vino")
    "vf" '(nil :which-key "find...")
    "vfa" '(vino-entry-find-file-available
            :which-key "available vino")
    "vfg" '(vino-grape-find-file :which-key "grape")
    "vfp" '(vino-producer-find-file :which-key "producer")
    "vfr" '(vino-region-find-file :which-key "region")
    "vn" '(vino-entry-create :which-key "create vino")
    "va" '(vino-acquire :which-key "acquire vino")
    "vc" '(vino-consume :which-key "consume vino")
    "vr" '(vino-entry-rate :which-key "rate vino"))
  :init
  (with-eval-after-load 'org-roam
    (vino-setup))
  (add-hook 'vino-entry-create-handle-functions #'vi-acquire)
  (setq-default
   vino-inventory-file (expand-file-name "wine.journal"
                                         vulpea-directory)
   vino-availability-fn #'vino-availability-get
   vino-availability-add-fn #'vino-availability-add
   vino-availability-sub-fn #'vino-availability-sub
   vino-producer-template '(:file-name "wine/producer/%<%Y%m%d%H%M%S>-${slug}.org"
                            :tags ("barberry/public"))
   vino-entry-template '(:file-name "wine/cellar/${id}.org"
                         :tags ("barberry/public"))
   vino-rating-template '(:file-name "wine/rating/${id}.org"
                          :tags ("barberry/public"))
   vino-region-template '(:file-name "wine/region/%<%Y%m%d%H%M%S>-${slug}.org"
                          :tags ("barberry/public"))
   vino-appellation-template '(:file-name "wine/appellation/%<%Y%m%d%H%M%S>-${slug}.org"
                               :tags ("barberry/public"))
   vino-grape-template '(:file-name "wine/grape/%<%Y%m%d%H%M%S>-${slug}.org"
                         :tags ("barberry/public"))
   vino-sources-fn #'vino-sources
   vino-rating-props
   '((1 . (("SCORE" . 3)))
     (2 . (("AROMA_QUALITY" . 3)
           ("AROMA_INTENSITY" . 2)
           ("AROMA_COMPLEXITY" . 3)
           ("BALANCE" . 3)
           ("FLAVOURS" . 2)
           ("AFTERTASTE" . 3)
           ("GENERAL" . 4)))
     (3
      .
      (("AROMA_QUALITY" .
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
        (("aroma can be perceived without putting nose into glass"
          .
          2)
         ("aroma can be perceived only by putting nose into glass"
          .
          1)
         ("closed, you need to put a lot of effort to get the aroma"
          .
          0)))

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
         ("great wine, will look into tasting it once more" . 3)
         ("good wine, will drink it again with pleasure" . 2)
         ("average wine, only with parents" . 1)
         ("bad wine, only for enemies" . 0))))))
   vino-rating-extra-meta (list
                           (list
                            :name "location"
                            :read-fn #'vino-select-location
                            :mode 'single
                            :type 'note)
                           (list
                            :name "convive"
                            :read-fn #'vino-select-convive
                            :mode 'multiple
                            :type 'note))
   brb-ledger-file (expand-file-name "barberry-garden.journal"
                                     vulpea-directory)))

(provide 'init-vino)
;;; init-vino.el ends here
