;;; packages-funcs.el --- d12frosted-core layer packages-funcs file for
;;; Spacemacs.
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

(defun d12/projectile-replace-regexp ()
  "Replace a string in the project using `tags-query-replace'.
Less efficient than `projectile-replace' but at least allows
usage of regular expressions. See
https://github.com/bbatsov/projectile/issues/576 for more details
on `projectile-replace' issue with regexps."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (files (-map (lambda (f) (concat (projectile-project-root) f)) (projectile-current-project-files))))
    (tags-query-replace old-text new-text nil (cons 'list files))))

(defun d12-ivy//add-files (list)
  (setq d12-ivy--files (-union d12-ivy--files list)))

(defun d12-ivy ()
  "Interesting files discovery with ivy interface."
  (interactive)
  (ivy-read "Config file: " d12-ivy--files
            :action '(1
                      ("o" find-file "Open file"))))

;; packages-funcs.el ends here
