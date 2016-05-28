;;; d12-dir-settings.el --- d12frosted-core layer d12-dir-settings file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; Grabbed from http://emacs.stackexchange.com/a/14784/5161

;;; Code:

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
