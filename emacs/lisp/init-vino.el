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
  :ensure (:host github :repo "d12frosted/vino")
  :defer t
  :commands (vino-setup)
  :general
  (leader-def
    "v" '(nil :which-key "vino...")
    "vv" '(vino-entry-find-file :which-key "find vino")
    "vi" '(vino-entry-insert :which-key "insert vino")
    "vIr" '(vino-insert-region :which-key "insert region")
    "vIa" '(vino-insert-appellation :which-key "insert appellation")
    "vf" '(nil :which-key "find...")
    "vfa" '(vino-inv-find-file-available :which-key "available vino")
    "vfg" '(vino-grape-find-file :which-key "grape")
    "vfp" '(vino-producer-find-file :which-key "producer")
    "vfr" '(vino-region-find-file :which-key "region")
    "vn" '(vino-entry-create :which-key "create vino")
    "va" '(vino-inv-acquire :which-key "acquire vino")
    "vc" '(vino-inv-consume :which-key "consume vino")
    "vr" '(vino-entry-rate :which-key "rate vino"))
  (general-define-key
   :keymaps 'vino-inv-ui-mode-map
   "b" '(vino-inv-ui-record-spending :which-key "record spending")
   "P" '(vino-inv-ui-print-info :which-key "display print info")
   "m" '(vino-inv-ui-mark :which-key "mark entry at point")
   "cu" '(vino-inv-ui-kill-url :which-key "copy link to the wine")
   "ci" '(vino-inv-ui-kill-wine-id :which-key "copy id of the wine"))
  :init
  (with-eval-after-load 'org-roam
    (vino-setup)
    (vino-inv-setup))
  (add-hook 'vino-entry-create-handle-functions #'vino-inv-acquire)
  (add-hook 'vino-entry-create-handle-functions #'vino-entry-assign-extra-meta)
  (add-hook 'vino-entry-create-handle-functions #'vino-entry-assign-social-links)
  (add-hook 'vino-rating-create-handle-functions #'vino-rating-assign-extra-meta)
  (add-hook 'vino-inv-acquire-handle-functions #'vino-inv-acquire-bottle-handler)
  (add-hook 'vino-inv-consume-handle-functions #'vino-inv-consume-bottle-handler)
  (setq-default
   vino-producer-template '(:file-name "wine/producer/%<%Y%m%d%H%M%S>-${slug}.org"
                            :tags ("barberry/public"))
   vino-entry-template '(:file-name "wine/cellar/${id}.org"
                         :tags ("barberry/public"))
   vino-rating-template '(:file-name "wine/rating/${id}.org"
                          :tags ("barberry/public"))
   vino-region-template '(:file-name "wine/region/${country}/%<%Y%m%d%H%M%S>-${slug}.org"
                          :tags ("barberry/public"))
   vino-appellation-template '(:file-name "wine/appellation/${country}/%<%Y%m%d%H%M%S>-${slug}.org"
                               :tags ("barberry/public"))
   vino-grape-template '(:file-name "wine/grape/%<%Y%m%d%H%M%S>-${slug}.org"
                         :tags ("barberry/public"))
   vino-origin-select-fn #'vino-origin-select-custom
   vino-entry-rating-average-method 'latest
   vino-entry-meta-props-order '("carbonation"
                                 "carbonation method"
                                 "colour"
                                 "sweetness"
                                 "producer"
                                 "name"
                                 "vintage"
                                 "base"
                                 "sur lie"
                                 "degorgee"
                                 "volume"
                                 "country"
                                 "region"
                                 "appellation"
                                 "subregion"
                                 "grapes"
                                 "alcohol"
                                 "sugar"
                                 "price"
                                 "price date"
                                 "price private"
                                 "acquired"
                                 "consumed"
                                 "available"
                                 "rating"
                                 "ratings"
                                 "externalId"
                                 "vivinoId"
                                 "vivino"
                                 "wineBureauId"
                                 "sabotage"
                                 "winewineId"
                                 "winewine"
                                 "images")
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
         ("bad wine, only for enemies" . 0)))))
     (4 . (("SCORE" . 5.0))))
   brb-ledger-file (expand-file-name "barberry-garden.journal"
                                     vulpea-directory)))

(use-package brb
  :ensure (:host github :repo "d12frosted/brb")
  :defer t
  :general
  (leader-def
    "vl" '(nil :which-key "ledger...")
    "vlo" '(brb-ledger-display :which "display")
    "vld" '(brb-ledger-display :which "deposit")
    "vls" '(brb-ledger-display :which "spend")))

(provide 'init-vino)
;;; init-vino.el ends here
