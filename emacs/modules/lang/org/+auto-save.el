;;; lang/org/+auto-save.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 30 Nov 2018
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

(advice-add #'org-agenda-refile :after #'+org*save-all)
(advice-add #'org-agenda-todo :after #'+org*save-all)
(advice-add #'org-agenda-clock-in :after #'+org*save-all)
(advice-add #'org-agenda-clock-out :after #'+org*save-all)
