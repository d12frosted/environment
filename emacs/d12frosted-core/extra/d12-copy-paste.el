;;; d12-copy-paste.el --- d12frosted-core layer d12-copy-paste file for
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

(defun d12/copy-line-or-region (&optional copy-func)
  "Copy current line (with newline character) or region. When
`universal-argument' is called first, copy whole buffer (but
respect `narrow-to-region').

When `copy-func' is provided, it is used to copy line or region
instead of `kill-ring-save'"
  (interactive)
  (d12//funcall-on-line-or-region
   (if (fboundp copy-func)
       copy-func
     'copy-region-as-kill)))

(defun d12/kill-line-or-region (&optional kill-func)
  "Cut current line or region. When `universal-argument' is
called first, cut whole buffer (but respect `narrow-to-region').

When `kill-func' is provided, it is used to copy line or region
instead of `kill-region'"
  (interactive)
  (d12//funcall-on-line-or-region
   (if (fboundp kill-func)
       kill-func
     'kill-region)))

(defun d12/delete-line-or-region (&optional delete-func)
  "Delete current line or region without putting it to kill-ring.
When `universal-argument' is called first, delete whole
buffer (but respect `narrow-to-region').

When `kill-func' is provided, it is used to copy line or region
instead of `kill-region'"
  (interactive)
  (d12//funcall-on-line-or-region
   (if (fboundp delete-func)
       delete-func
     'delete-region)))

(defun d12//funcall-on-line-or-region (func)
  "Call function `f' on current line or region."
  (if current-prefix-arg
      (funcall func (point-min) (point-max))
    (if (use-region-p)
        (funcall func (region-beginning) (region-end) t)
      (funcall func (line-beginning-position) (line-beginning-position 2) nil))))

(provide 'd12-copy-paste)

;;; d12-files.el ends here
