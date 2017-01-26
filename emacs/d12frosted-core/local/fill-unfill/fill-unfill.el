;;; fill-unfill.el --- fill or unfill paragraphs

;; Copyright (c) 2015-2017 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 07 Jun 2016

;; Keywords:
;; Homepage:

;; Package-Version: 0.0.1
;; Package-Requires: ()

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; Cold-bloodedly stolen from
;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
;;

;;; Code:
;;

;;;###autoload
(defun d12-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'd12-fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'d12-fill-or-unfill)

(provide 'fill-unfill)

;;; fill-unfill.el ends here
