;;; d12frosted.el --- init file for own configurations
;;
;; Copyright (C) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;;
;;; Commentary:
;;
;;; Code:

;; setup some constants and variables
(defconst d12-path/dropbox (concat user-home-directory "Dropbox/"))
(defconst d12-path/xdg-config (concat (getenv "XDG_CONFIG_HOME") "/"))
(defconst d12-path/emacs-home (expand-file-name user-emacs-directory))
(defconst d12-path/emacs-layers (concat d12-path/xdg-config "emacs/"))
(defconst d12-path/emacs-private (concat d12-path/dropbox "Apps/Emacs/"))
(defconst d12-path/spacemacs-init (dotspacemacs/location))
(defconst d12-path/d12frosted-init
  (concat (file-name-directory d12-path/spacemacs-init) "d12frosted.el"))
(defconst d12-path/fish-public (concat d12-path/xdg-config "fish/"))
(defconst d12-path/fish-private (concat d12-path/dropbox "Apps/fish/"))
(defconst d12-path/developer (concat user-home-directory "Developer/"))
(defconst d12-path/org-home (concat d12-path/dropbox "org/"))

;; setup custom-file
(setq custom-file (concat d12-path/emacs-private "custom.el"))
(load custom-file t)

;; load `private.el' file containing all the sensitive data
(load (concat d12-path/emacs-private "private.el"))

;;; d12frosted.el ends here
