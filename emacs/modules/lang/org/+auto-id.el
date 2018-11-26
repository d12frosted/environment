;;; lang/org/+auto-id.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 26 Nov 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Automatically add ID and CUSTOM_ID in Org files.
;;
;; based on https://writequit.org/articles/emacs-org-mode-generate-ids.html
;;
;;; Code:

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(add-hook 'before-save-hook #'+org-auto-id-add-to-headlines-in-file)
(add-hook 'org-capture-prepare-finalize-hook #'+org-auto-id-dwim)
