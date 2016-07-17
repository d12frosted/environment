;;; packages.el --- d12frosted-window-purpose layer packages file for Spacemacs.
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

(defconst d12frosted-window-purpose-packages
  '(window-purpose))

(defun d12frosted-window-purpose/post-init-window-purpose ()
  (use-package window-purpose
    :init
    (setq purpose-layout-dirs
          (list d12frosted-window-purpose-layout-dir))
    :config
    (add-to-list 'purpose-user-mode-purposes '(esh-mode . terminal))
    (purpose-compile-user-configuration)))

;;; packages.el ends here
