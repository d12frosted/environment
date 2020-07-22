;;; init-ivy.el --- best completion system -*- lexical-binding: t; -*-
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

(require 'init-project)
(require 'init-keybindings)
(require 'init-package)
(require 'subr-x)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :commands (counsel-find-file
             counsel-file-jump)
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
  :config
  (setq
   ivy-initial-inputs-alist
   '((counsel-minor . "^+")
		 (counsel-package . "^+")
		 (counsel-org-capture . "^")
		 (counsel-M-x . "^+?")
		 (counsel-describe-function . "^+?")
		 (counsel-describe-variable . "^+?"))))

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :defines (prescient-filter-method
            prescient-save-file
            ivy-prescient-retain-classic-highlighting)
  :commands (prescient-persist-mode)
  :init
  (setq prescient-filter-method '(literal regexp initialism)
        ivy-prescient-retain-classic-highlighting t)
  :config
  (setq prescient-save-file (concat +path-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package ivy-rich
  :after ivy
  :commands (ivy-rich-mode)
  :init
  (ivy-rich-mode))

(provide 'init-ivy)
;;; init-ivy.el ends here
