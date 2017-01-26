;;; mobile-app-installer.el --- interface for installing mobile applications

;; Copyright (c) 2015-2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 22 Jul 2016

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ((f "0.18.2") (dash "2.12"))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(require 'f)
(require 'dash)

(defvar mai-directory-list '()
  "List of directories containing applications.")

(defun mai-install-cmd (application)
  (pcase (file-name-extension application)
    ("ipa" (format "%s %s" (executable-find "ipa-install") (shell-quote-argument application)))
    ("apk" (format "%s install -r -d %s" (executable-find "adb") (shell-quote-argument application)))
    (ext (error "Applications with extension '%s' are not supported" ext))))

;;;###autoload
(defun mai-install (application &optional dry-run)
  "Install APPLICATION."
  (interactive "f")
  (if dry-run
      (message (mai-install-cmd application))
    (async-shell-command (mai-install-cmd application))))

;;;###autoload
(defun mai ()
  (interactive)
  (let* ((apps
          (-flatten (mapcar
                     (lambda (dir)
                       (f-files dir
                                (lambda (file)
                                  (or (f-ext? file "ipa")
                                      (f-ext? file "apk")))))
                     mai-directory-list)))
         (app (completing-read "Choose app: " apps)))
    (mai-install app)))

(provide 'mobile-app-installer)

;;; mobile-app-installer.el ends here
