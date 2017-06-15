;;; d12-interesting-files.el ---

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

(defvar d12-interesting-files-list '()
  "List of interesting files to discover via specified interface.")

(defvar d12-interesting-files-interface #'d12-interesting-files-completing-read
  "Interface to use as completion.")

;;;###autoload
(defun d12-interesting-files-add (list)
  "Add files to `d12-interesting-files-list'"
  (setq d12-interesting-files-list (append d12-interesting-files-list list)))

;;;###autoload
(defun d12-interesting-files ()
  "Discover interesting files."
  (interactive)
  (funcall d12-interesting-files-interface))

;;;; Ivy interface
;;

;;;###autoload
(defun d12-interesting-files-ivy ()
  "Interesting files discovery with ivy interface."
  (ivy-read "Interesting file: " d12-interesting-files-list
            :action '(1
                      ("o" find-file "Open file"))))

;;;; Helm interface
;;

;;;###autoload
(defun d12-interesting-files-helm ()
  "Interesting files discovery with helm interface."
  (helm :buffer "*helm: d12frosted"
        :sources `((name . "Interesting files")
                   (candidates . d12-interesting-files-list)
                   (candidate-number-limit)
                   (action . (("Open file" . find-file))))))

;;;; Completing read interface
;;

;;;###autoload
(defun d12-interesting-files-completing-read ()
  "Interesting files discovery with generic interface."
  (completing-read "Interesting file: " d12-interesting-files-list))

(provide 'd12-interesting-files)

;;; d12-interesting-files.el ends here
