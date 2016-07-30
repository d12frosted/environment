;;; d12-files.el ---

;; Copyright (c) 2016 Boris Buliga

;; Author: Boris Buliga <d12frosted@gmail.com>
;; Maintainer: Boris Buliga <d12frosted@gmail.com>
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
