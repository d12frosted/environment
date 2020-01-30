;;; init-puml.el --- tools for working with Plant UML -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 30 Jan 2020
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

(require 'init-path)
(require 'init-package)
(require 'init-org)

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :defines (plantuml-jar-path
            plantuml-default-exec-mode)
  :commands plantuml-download-jar
  :init
  (setq plantuml-jar-path (concat +path-etc-dir "plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  :config
  (setq plantuml-default-exec-mode
        (cond ((executable-find "plantuml") 'executable)
              ((file-exists-p plantuml-jar-path) 'jar)
              (plantuml-default-exec-mode))))

(use-package flycheck-plantuml
  :after plantuml-mode
  :commands (flycheck-plantuml-setup)
  :config (flycheck-plantuml-setup))

(provide 'init-puml)
;;; init-puml.el ends here
