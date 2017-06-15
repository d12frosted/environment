;;; config.el --- d12frosted-theming layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
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
    (org-agenda-done :height 1.0)
    (org-agenda-structure :height 1.3
                          :foreground "#3a81c3"
                          :underline t))))

;;; config.el ends here
