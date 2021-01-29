;;; init-fun.el --- fun activities are fun -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Nov 2019
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

(require 'init-package)

(use-package roguel-ike
  :commands roguel-ike
  :custom (roguel-ike-save-directory (concat +path-cache-dir "roguel-ike/")))

(use-package telega
  :defer t)

(provide 'init-fun)
;;; init-fun.el ends here
