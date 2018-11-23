;;; lang/rest/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
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

(def-package! restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  ;; Forces underlying SSL verification to prompt for self-signed or
  ;; invalid certs, rather than silently reject them.
  (defun +rest*permit-self-signed-ssl (orig-fn &rest args)
    (let (gnutls-verify-error tls-checktrust)
      (apply orig-fn args)))
  (advice-add #'restclient-http-do :around #'+rest*permit-self-signed-ssl))

(def-package! company-restclient
  :when (featurep! :completion company)
  :after restclient
  :config (set-company-backend! 'restclient-mode 'company-restclient))
