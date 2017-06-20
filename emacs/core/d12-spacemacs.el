;;; d12-spacemacs.el --- d12-spacemacs file for personal configurations -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2017 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; Spacemacs loading module.
;;
;; Call `d12-spacemacs-load` in order to load Spacemacs.
;;
;;; Code:

(defun d12-spacemacs-load (distr-home distr-init-file user-config-file)
  "Load Spacemacs distribution."
  (setq-default
   spacemacs-start-directory distr-home
   dotspacemacs-filepath user-config-file)
  (message "Loading Spacemacs: " distr-init-file)
  (load-file distr-init-file))

(provide 'd12-spacemacs)
;;; d12-spacemacs.el ends here
