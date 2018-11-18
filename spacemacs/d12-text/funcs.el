;;; funcs.el --- d12-text layer funcs file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defun fancy-yank-extract-github-link (url owner repo type number &rest args)
  (list url
        (concat
         (if (or (string-equal d12-text-github-user owner)
                 (seq-contains d12-text-known-github-owners owner))
             repo
           (concat owner "/" repo))
         (if (string-equal type "milestone")
             "m"
           "#")
         number)
        (when (string-empty-p (buffer-substring (point) (line-end-position)))
          (concat " - "
                  (d12-git/get-issue-title owner repo type number)))))

;;; funcs.el ends here