;;; ui/modern/packages.el -*- no-byte-compile: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Nov 2018
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

(package! all-the-icons)
(package! highlight-numbers)
(package! highlight-escape-sequences
  :recipe (:fetcher github :repo "hlissner/highlight-escape-sequences"))
(package! rainbow-delimiters)