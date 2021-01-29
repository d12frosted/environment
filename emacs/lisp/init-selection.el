;;; init-selection.el --- selecting candidates -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 03 Aug 2020
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

(require 'init-project)
(require 'init-keybindings)
(require 'init-package)
(require 'subr-x)

(defvar +selection-system 'consult
  "Selection system to use: ivy, selectrum or consult.")

(use-package counsel
  :if (eq +selection-system 'ivy)
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
  :if (eq +selection-system 'ivy)
  :hook (ivy-mode . ivy-prescient-mode)
  :defines (ivy-prescient-retain-classic-highlighting
            ivy-prescient-sort-commands)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer counsel-grep
               counsel-git-grep counsel-ag counsel-rg counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf)
        ivy-prescient-retain-classic-highlighting t))

(use-package ivy-rich
  :disabled
  :if (eq +selection-system 'ivy)
  :after ivy
  :commands (ivy-rich-mode)
  :init
  (ivy-rich-mode))

(use-package selectrum
  :if (or (eq +selection-system 'selectrum)
          (eq +selection-system 'consult))
  :hook ((after-init . selectrum-mode)))

(use-package selectrum-prescient
  :if (or (eq +selection-system 'selectrum)
          (eq +selection-system 'consult))
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package ctrlf
  :if (eq +selection-system 'selectrum)
  :hook (selectrum-mode . ctrlf-mode))

(use-package prescient
  :defer t
  :commands (prescient-persist-mode
             prescient--with-group
             prescient--initials-regexp
             prescient--fuzzy-regexp
             prescient-split-query)
  :defines (prescient-filter-method prescient-save-file)
  :init
  (setq prescient-filter-method '(literal regexp initialism))
  :config
  (setq prescient-save-file (concat +path-cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package consult
  :if (eq +selection-system 'consult))

(use-package embark-consult
  :if (eq +selection-system 'consult))

(use-package marginalia
  :if (eq +selection-system 'consult)
  :init
  (marginalia-mode))

(provide 'init-selection)
;;; init-selection.el ends here
