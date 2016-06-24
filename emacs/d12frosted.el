;;; d12frosted.el --- init file for own configurations
;;
;; Copyright (C) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;;
;;; Commentary:
;;
;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; setup some constants and variables
(defconst package--initialized nil)
(defconst user-home-directory (expand-file-name "~/"))
(defconst d12-path/dropbox (concat user-home-directory "Dropbox/"))

(defconst d12-path/xdg-config (concat (getenv "XDG_CONFIG_HOME") "/"))

(defconst d12-path/emacs-home (concat d12-path/xdg-config "emacs/"))
(defconst d12-path/emacs-private (concat d12-path/dropbox "Apps/Emacs/"))

(defconst d12-path/d12frosted-init
  (concat d12-path/emacs-home "d12frosted.el"))

(defconst d12-path/spacemacs-layers d12-path/emacs-home)
(defconst d12-path/spacemacs-user-config
  (concat d12-path/emacs-home "spacemacs.el"))
(defconst d12-path/spacemacs-home (concat user-home-directory ".spacemacs/"))
(defconst d12-path/spacemacs-init
  (concat d12-path/spacemacs-home "init.el"))

(defconst d12-path/fish-public (concat d12-path/xdg-config "fish/"))
(defconst d12-path/fish-private (concat d12-path/dropbox "Apps/fish/"))

(defconst d12-path/developer (concat user-home-directory "Developer/"))

(defconst d12-path/org-home (concat d12-path/dropbox "org/"))

;; setup custom-file
(setq custom-file (concat d12-path/emacs-private "custom.el"))
(load custom-file t)

;; setup package-user-dir to allow seamless switch between emacs versions
(setq package-user-dir (file-name-as-directory
                        (concat d12-path/spacemacs-home "elpa/" emacs-version)))

;; load `private.el' file containing all the sensitive data
(load (concat d12-path/emacs-private "private.el"))

;; load spacemacs
(setq-default spacemacs-start-directory d12-path/spacemacs-home
              dotspacemacs-filepath d12-path/spacemacs-user-config)
(load-file d12-path/spacemacs-init)

;;; d12frosted.el ends here
