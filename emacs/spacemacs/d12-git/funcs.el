;;; funcs.el --- d12-git layer funcs file for Spacemacs. -*- lexical-binding: t; -*-
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

(defun d12-git/get-issue-title (owner repo type number)
  "Get the title of the issue/pr."
  (let ((ptype (cond
                ((string-prefix-p "issue" type) "issues")
                ((string-prefix-p "pull" type) "pulls")
                ((string-prefix-p "pr" type) "pulls")
                (t (user-error "unsupported issue type: %s" type)))))
    (alist-get 'title
               (ghub-get (format "/repos/%s/%s/%s/%s"
                                 owner repo ptype number)))))

;;; funcs.el ends here
