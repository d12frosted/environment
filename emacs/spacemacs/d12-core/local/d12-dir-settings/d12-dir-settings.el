;;; d12-dir-settings.el --- d12-dir-settings file for personal configurations -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2017 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; Package-Version: 0.0.1
;; Package-Requires: ()
;;
;; This file is not part of GNU Emacs.
;;; License: GPLv3
;;
;;; Commentary:
;;
;; Directory settings *without* security prompt and most importantly, avoiding
;; customise mechanic.
;;
;; Usage
;;
;;   (add-hook 'find-file-hook #'d12-dir-settings/load)
;;
;; Author of the solution is Geier.
;; https://emacs.stackexchange.com/questions/14753/white-list-of-dir-locals-el
;;
;;; Code:

(defvar d12-dir-settings/file-name
  "settings.el"
  "File containing dir settings.")

;;;###autoload
(defun d12-dir-settings/recursive-load (file)
  "Load settings file for FILE."
  (let ((lds-dir (locate-dominating-file file d12-dir-settings/file-name)))
    (when lds-dir
      (progn
        (load-file (concat lds-dir d12-dir-settings/file-name))
        (d12-dir-settings/recursive-load (file-truename (concat lds-dir "..")))))))

;;;###autoload
(defun d12-dir-settings/load ()
  "Load settings file for current buffer."
  (interactive)
  (when buffer-file-name
    (d12-dir-settings/recursive-load buffer-file-name)))

(provide 'd12-dir-settings)
;;; d12-dir-settings.el ends here
