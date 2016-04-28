;;; d12-ivy.el --- d12frosted-core layer d12-ivy file for Spacemacs.
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

(require 'dash)

(defvar d12-ivy--sources '())

(setq d12-ivy--config-files `(,(concat d12-path/emacs-layers "init.el")
                              ,(concat d12-path/emacs-private "private.el")
                              ,(concat d12-path/fish-public "config.fish")
                              ,(concat d12-path/fish-private "preconfig.fish")
                              ,(concat d12-path/fish-private "postconfig.fish")
                              ,(concat d12-path/emacs-home "init.el")))

(defun d12-ivy//add-files (list)
  (setq d12-ivy--sources (-union d12-ivy--sources list)))

(defun d12-ivy ()
  "Interesting files discovery with ivy interface."
  (interactive)
  (ivy-read "Config file: " d12-ivy--sources
            :action '(1
                      ("o" find-file "Open file"))))

(d12-ivy//add-files d12-ivy--config-files)

(provide 'd12-ivy)

;;; d12-helm.el ends here
