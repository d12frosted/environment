;;; d12-org-capture-url.el --- Parse and capture URL -*- lexical-binding: t; -*-
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
;;; Code:

;; (require 'cl-lib)

;; (cl-defstruct (d12-org-capture-url-data (:constructor d12-org-capture--url-data-create))
;;   )

(defun d12-org-capture-url (url)
  "Format url for capture template."
  (cond
   ((string-match
     "https://github.com/\\([[:alnum:]]+\\)/\\([[:alnum:]\-]+\\)/[[:alpha:]]+/\\([[:digit:]]+\\)#?.*"
     url)
    (format "[[%s/%s#%s][%s]]"
            (match-string 1 url)
            (match-string 2 url)
            (match-string 3 url)
            url)
    )
   (t (format "link: %s" url))
   ))

;; (d12-org-capture-url "https://github.com/d12frosted/homebrew-emacs-plus/issues/42")

(provide 'd12-org-capture-url)

;;; d12-org-capture-url.el ends here
