;;; d12-dir-settings.el ---

;; Copyright (c) 2015-2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 29 Jul 2016

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(defvar d12-dir-settings/file
  "settings.el"
  "File containing dir settings.")

(defun d12-dir-settings/recursive-load (currentfile)
  (let ((lds-dir (locate-dominating-file currentfile d12-dir-settings/file)))
    (when lds-dir
      (progn
        (load-file (concat lds-dir d12-dir-settings/file))
        (d12-dir-settings/recursive-load (file-truename (concat lds-dir "..")))))))

(defun d12-dir-settings/load ()
  (interactive)
  (when buffer-file-name
    (d12-dir-settings/recursive-load buffer-file-name)))

(add-hook 'find-file-hook 'd12-dir-settings/load)

(provide 'd12-dir-settings)

;;; d12-dir-settings.el ends here
