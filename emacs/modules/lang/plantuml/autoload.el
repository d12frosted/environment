;;; lang/plantuml/autoload.el -*- lexical-binding: t; -*-
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

;;;###autoload
(defun +plantuml/install ()
  "Install plantuml.jar."
  (interactive)
  (if (file-exists-p plantuml-jar-path)
      (user-error "plantuml.jar already installed")
    (url-copy-file "https://kent.dl.sourceforge.net/project/plantuml/plantuml.jar"
                   plantuml-jar-path)))
