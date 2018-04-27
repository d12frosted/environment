;;; config.el --- d12-org layer config file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defvar d12-org--mobile-sync-timer nil)
(defvar d12-org-mobile-sync-idle-secs (* 60 5))
(defvar d12-org-refile-ignore-tags '("JOURNAL" "REFILE"))
(defvar d12-org-hide-scheduled-and-waiting-next-tasks t)

;;; config.el ends here
