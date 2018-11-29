;;; lang/org/packages.el -*- no-byte-compile: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 21 Nov 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(package! org-plus-contrib)
(package! org :ignore t)
(package! org-brain)
(package! orgability
  :recipe (:fetcher github :repo "d12frosted/orgability"))
(package! org-board)
(package! org-download)
