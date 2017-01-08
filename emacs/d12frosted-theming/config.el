;;; config.el --- d12frosted-theming layer packages file for Spacemacs.
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

(setq-default
 theming-modifications
 '((spacemacs-light
    ;; Font locking
    (font-lock-preprocessor-face :background "#F3F3B2")
    (whitespace-tab :foreground "#F6ECE4")

    ;; org-mode
    (org-agenda-date :height 1.3)
    (org-agenda-date-weekend :height 1.3)
    (org-agenda-date-today :height 1.3
                           :underline t)
    (org-scheduled-today :height 1.0)
    (org-agenda-done :height 1.0))))

;;; config.el ends here
