;;; init-rest.el --- init restclient -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 03 Jan 2020
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
(require 'init-completion)

(use-package restclient
  :defer t
  :commands (restclient-mode)
  :mode ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :after restclient
  :init (+company-set-backend 'restclient-mode 'company-restclient))

(provide 'init-rest)
;;; init-rest.el ends here
