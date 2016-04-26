;;; d12-files.el --- d12frosted-core layer d12-files file for Spacemacs.
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

;;; Code:

(defun d12-files/run-find (dir query)
  (split-string (shell-command-to-string (concat "find " dir " " query)) "\n" t))

(defun d12-files/query (query dir &optional maxdepth type)
  (let ((-type (concat "-type " (if type type "f")))
        (-query (concat "-name '" query "'"))
        (-maxdepth (if maxdepth (concat " -maxdepth " (number-to-string maxdepth)) "")))
    (d12-files/run-find (directory-file-name dir) (concat -type " " -query " " -maxdepth))))

(defun d12/get-string-from-file (filepath)
  "Return filepath's file content."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(provide 'd12-files)

;;; d12-files.el ends here
