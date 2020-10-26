;;; init-ctags.el --- frying ctags -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 26 May 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-package)

(defvar +ctags-file-name "TAGS"
  "Location for tags file produced by 'ctags'.")

(defun +ctags-create (dir-name lang &optional force)
  "Generate tags file in DIR-NAME.

Only files matching LANG are processed.

Returns path to generated file.

When FORCE is non-nil, tags are not generated if tags file
already exists."
  (let ((file (expand-file-name +ctags-file-name dir-name)))
    (unless (and (null force) (file-exists-p file))
      (shell-command
       (format "%s -f %s/%s -e -R %s --languages=%s"
               (executable-find "ctags")
               dir-name
               +ctags-file-name
               (directory-file-name dir-name)
               lang)))
    file))

(defun +ctags-enable-auto-update ()
  "Enable TAGS file automatic update."
  (add-hook 'after-save-hook
            'counsel-etags-update-tags-force 'append 'local))

(use-package etags
  :straight nil
  :defer t
  :init
  (setq tags-revert-without-query t))

(use-package counsel-etags
  :defer t
  :commands (counsel-etags-locate-tags-file)
  :init
  (setq counsel-etags-update-interval 10)
  :config
  (push "build" counsel-etags-ignore-directories))

(use-package company-ctags
  :defer t)

(defun +ctags-set-files (files)
  "Setup list of tags FILES."
  (setq-local tags-table-list files)
  (setq-local counsel-etags-extra-tags-files files)
  (setq-local company-ctags-extra-tags-files files))

(defun +ctags-cons (tags-file)
  "Add TAGS-FILE to the tags list.

TAGS-FILE must be absolute path to the tags file."
  (+ctags-set-files (cons tags-file tags-table-list)))

(defun +ctags-append (files)
  "Append tags FILES to the tags list.

Each element of FILES must be absolute path to the tags file."
  (+ctags-set-files (append tags-table-list files)))

(provide 'init-ctags)
;;; init-ctags.el ends here
