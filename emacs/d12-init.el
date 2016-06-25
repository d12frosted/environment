;;; d12-init.el --- init file for own configurations
;;
;; Copyright (C) 2016 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@gmail.com>
;;
;;; Commentary:
;;
;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; setup some constants and variables
(defconst package--initialized nil)
(defconst user-home-directory (expand-file-name "~/"))
(defconst d12-path/dropbox (concat user-home-directory "Dropbox/"))

(defconst d12-path/xdg-config (concat (getenv "XDG_CONFIG_HOME") "/"))

(defconst d12-path/emacs-home (concat d12-path/xdg-config "emacs/"))
(defconst d12-path/emacs-private (concat d12-path/dropbox "Apps/Emacs/"))

(defconst d12-path/d12frosted-init
  (concat d12-path/emacs-home "d12-init.el"))

(defconst d12-path/spacemacs-layers d12-path/emacs-home)
(defconst d12-path/spacemacs-user-config
  (concat d12-path/emacs-home "spacemacs.el"))
(defconst d12-path/spacemacs-home (concat user-home-directory ".spacemacs/"))
(defconst d12-path/spacemacs-init
  (concat d12-path/spacemacs-home "init.el"))

(defconst d12-path/fish-public (concat d12-path/xdg-config "fish/"))
(defconst d12-path/fish-private (concat d12-path/dropbox "Apps/fish/"))

(defconst d12-path/developer (concat user-home-directory "Developer/"))

(defconst d12-path/org-home (concat d12-path/dropbox "org/"))

;; setup custom-file
(setq custom-file (concat d12-path/emacs-private "custom.el"))
(load custom-file t)

;; setup package-user-dir to allow seamless switch between emacs versions
(setq package-user-dir (file-name-as-directory
                        (concat d12-path/spacemacs-home "elpa/" emacs-version)))

;; load `private.el' file containing all the sensitive data
(load (concat d12-path/emacs-private "private.el"))

(defun d12-layers/add-extra-to-load-path (layer)
  "Add 'extra' folder to `load-path' for a given LAYER.

Load-path is modified only when such folder exists."
  (let* ((layer-path (configuration-layer/get-layer-path layer))
         (layer-root (format "%s%s/" layer-path layer))
         (extra-path (concat layer-root "extra/")))
    (when (file-exists-p extra-path)
      (add-to-load-path extra-path))))

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

;; load spacemacs
(setq-default spacemacs-start-directory d12-path/spacemacs-home
              dotspacemacs-filepath d12-path/spacemacs-user-config)
(load-file d12-path/spacemacs-init)

;; keybindings

;;; d12-init.el ends here
