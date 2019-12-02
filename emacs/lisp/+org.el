;;; +org.el --- general purpose utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 01 Nov 2019
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

;;;###autoload
(defmacro +org-with-file (file &rest body)
  "Execute BODY in `org-mode' FILE."
  `(with-current-buffer (find-file-noselect ,file)
     ,@body))

;;;###autoload
(defun +org-parent-id ()
  "Return parent id of entry at point."
  (save-excursion
    (when (org-up-heading-safe)
      (org-id-get-create))))

(provide '+org)
;;; +org.el ends here
