;;; d12-spacemacs.el --- d12-spacemacs file for personal configurations
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

(defun d12-layers/package-used-by-layer-p (layer package)
  (not (null (memq package
                   (configuration-layer/get-layer-property layer :packages)))))

(defun d12-layers/get-buffer-layer-name ()
  (let* ((file (buffer-file-name) )
         (name (file-name-base (directory-file-name (file-name-directory file))))
         (layers (configuration-layer/get-layers-list))
         (layer nil))
    (mapc (lambda (l)
            (when (string-equal name (symbol-name l))
              (setq layer l)))
          layers)
    layer))

(defmacro d12-layers|define-layer-package (layer package type &rest args)
  "Define package for specified layer.

Usage: (d12-layers|define-layer-package layer package pre|post|nil)."
  (let* ((ftype (cond
                 ((eq 'type 'pre) "pre-")
                 ((eq 'type 'post) "post-")
                 (t "")))
         (fname (intern (format "%s/%sinit-%s"
                                layer
                                ftype
                                package)))
         (feature-init (intern (format "%s-%s-init" layer package)))
         (feature-config (intern (format "%s-%s-config" layer package)))
         (package-config args))
    (add-to-list 'package-config :init t)
    (add-to-list 'package-config `(require ',feature-init nil t) t)
    (add-to-list 'package-config :config t)
    (add-to-list 'package-config `(require ',feature-config nil t) t)
    (message "feature-init = %s" feature-init)
    `(defun ,fname ()
       (use-package ,package ,@package-config))))

(provide 'd12-spacemacs)
;;; d12-spacemacs.el ends here
