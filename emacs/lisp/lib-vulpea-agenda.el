;;; lib-vulpea-agenda.el --- Utilities for building agenda -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 12 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The agenda machinery now lives in vulpea-para (vulpea-para-agenda-*):
;; the open-work check and agenda-file derivation, the project/task
;; predicates, the skip functions, the cmd-* command building blocks,
;; the Area > Project category formatter, and the main/person/area
;; commands.  See init-vulpea.el for how they are assembled into
;; `org-agenda-custom-commands' and wired up.
;;
;;; Code:

(provide 'lib-vulpea-agenda)
;;; lib-vulpea-agenda.el ends here
