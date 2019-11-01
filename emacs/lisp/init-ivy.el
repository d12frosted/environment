;;; lisp/init-ivy.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("M-x" . counsel-M-x))
  :init
  (setq
   enable-recursive-minibuffers t
   ivy-use-selectable-prompt t
   ivy-use-virtual-buffers t
   ivy-height 10
   ivy-count-format "(%d/%d) "
   ivy-on-del-error-function nil)

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  :config
  (setq
   ivy-initial-inputs-alist '((counsel-minor . "^+")
			      (counsel-package . "^+")
			      (counsel-org-capture . "^")
			      (counsel-M-x . "^+?")
			      (counsel-describe-function . "^+?")
			      (counsel-describe-variable . "^+?"))))

(use-package counsel-projectile
  :commands (counsel-projectile-find-file
	     counsel-projectile-find-dir
	     counsel-projectile-switch-to-buffer
             counsel-projectile-grep
	     counsel-projectile-ag
	     counsel-projectile-switch-project)
  :init
  (global-set-key [remap projectile-find-file]        #'+ivy/projectile-find-file)
  (global-set-key [remap projectile-find-dir]         #'counsel-projectile-find-dir)
  (global-set-key [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
  (global-set-key [remap projectile-grep]             #'counsel-projectile-grep)
  (global-set-key [remap projectile-ag]               #'counsel-projectile-ag)
  (global-set-key [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))

;;;###autoload
(defun +ivy/projectile-find-file ()
  "A dwim version of `counsel-projectile-find-file'.

This version reverts to:

- `counsel-find-file' when invoked from $HOME;
- `counsel-file-jump' when invoked from a non-project;
- `projectile-find-file' when in a bug project (more than
  `ivy-sort-max-size' files);
- `counsel-projectile-find-file' otherwise.

The point of this is to avoid Emacs locking up indexing massive
file trees."
  (interactive)
  (call-interactively
   (cond ((or (file-equal-p default-directory "~")
              (when-let* ((proot (+project-root)))
			 (file-equal-p proot "~")))
          #'counsel-find-file)

         ((+project-p)
          (let ((files (projectile-current-project-files)))
            (if (<= (length files) ivy-sort-max-size)
                #'counsel-projectile-find-file
              #'projectile-find-file)))

         (#'counsel-file-jump))))

(provide 'init-ivy)
;;; init-ivy.el ends here
