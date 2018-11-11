;;; bb-spacemacs.el --- bb-spacemacs file for personal configurations -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
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
;; Call `bb:spacemacs-load` in order to load Spacemacs.
;;
;;; Code:

(defun bb:spacemacs-load (distr-home distr-init-file user-config-file)
  "Load Spacemacs distribution."
  (setq-default
   spacemacs-start-directory distr-home
   dotspacemacs-filepath user-config-file)
  (message "Loading Spacemacs: " distr-init-file)
  (load-file distr-init-file))

(provide 'bb-spacemacs)

;;; bb-spacemacs.el ends here
