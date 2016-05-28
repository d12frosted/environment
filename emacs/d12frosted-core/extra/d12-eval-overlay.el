;;; d12-eval-overlay.el --- d12frosted-core layer d12-eval-overlay file for Spacemacs.
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
;; Grabbed from http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html

;;; Code:

(add-to-list 'dotspacemacs-additional-packages 'cider)

(autoload 'cider--make-result-overlay "cider-overlays")

(defun d12/eval-overlay (value point)
  (require 'cider-overlays)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (d12/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (d12/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (d12/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

(provide 'd12-eval-overlay)

;;; d12-eval-overlay.el ends here
