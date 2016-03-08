;;; d12-helm.el --- d12frosted-core layer d12-helm file for Spacemacs.
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

(defvar d12-helm/sources '())

(defun d12-helm ()
  "Interesting files discovery with helm interface."
  (interactive)
  (helm :buffer "*helm: d12frosted"
        :sources (-map 'funcall d12-helm/sources)))

;; Sources

(defun d12-helm/config-source ()
  "Construct helm source from `d12-org/files-list'."
  `((name . "config files")
    (candidates . (,(concat d12-path/emacs-layers "init.el")
                   ,(concat d12-path/emacs-private "private.el")
                   ,(concat d12-path/fish-public "config.fish")
                   ,(concat d12-path/fish-private "preconfig.fish")
                   ,(concat d12-path/fish-private "postconfig.fish")
                   ,(concat d12-path/emacs-home "init.el")))
    (candidate-number-limit)
    (action . (("Open file" . find-file)))))

(provide 'd12-helm)

;;; d12-helm.el ends here
