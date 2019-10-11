;;; lang/plantuml/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 28 Nov 2018
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

(def-package! plantuml-mode
  :mode "\\.puml\\'"
  :init
  (setq plantuml-jar-path (concat nucleus-etc-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path
        plantuml-output-type "svg"
        plantuml-default-exec-mode 'jar)
  ;; TODO popup rules
  ;; :config
  ;; (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0)
  )


(def-package! flycheck-plantuml
  :when (featurep! :tools syntax-checker)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))
