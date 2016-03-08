;;; packages.el --- d12frosted-core layer packages file for Spacemacs.
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

(defconst d12frosted-core-packages
  '(beacon))

(defun d12frosted-core/init-beacon ()
  (use-package beacon
    :init
    (beacon-mode 1)))

;;; packages.el ends here
