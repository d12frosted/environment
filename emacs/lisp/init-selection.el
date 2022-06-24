;;; init-selection.el --- Selection configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Selection system configurations. Choose from ivy, selectrum and
;; consult.
;;
;;; Code:

(require 'config-path)

(defvar selection-system 'consult
  "Selection system to use: ivy, selectrum or consult.")

(use-package counsel
  :if (eq selection-system 'ivy)
  :diminish ivy-mode counsel-mode
  :commands (counsel-find-file
             counsel-file-jump)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :general
  (leader-def
    "iu" '(counsel-unicode-char :which-key "Unicode character"))
  :bind
  (("M-x" . counsel-M-x)
   ("C-h F" . counsel-faces))
  :init
  (setq
   enable-recursive-minibuffers t
   ivy-use-selectable-prompt t
   ivy-use-virtual-buffers t
   ivy-height 10
   ivy-count-format "(%d/%d) "
   ivy-on-del-error-function nil)
  :config
  (setq ivy-initial-inputs-alist
        '((counsel-minor . "^+")
          (counsel-package . "^+")
          (counsel-org-capture . "^")
          (counsel-M-x . "^+?")
          (counsel-describe-function . "^+?")
          (counsel-describe-variable . "^+?"))))

(use-package ivy-prescient
  :if (eq selection-system 'ivy)
  :hook (ivy-mode . ivy-prescient-mode)
  :defines (ivy-prescient-retain-classic-highlighting
            ivy-prescient-sort-commands)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer
               counsel-grep counsel-git-grep counsel-ag
               counsel-rg counsel-imenu counsel-yank-pop
               counsel-recentf counsel-buffer-or-recentf)
        ivy-prescient-retain-classic-highlighting t))

(use-package ivy-rich
  :disabled
  :if (eq selection-system 'ivy)
  :after ivy
  :commands (ivy-rich-mode)
  :init
  (ivy-rich-mode))

(use-package selectrum
  :if (or (eq selection-system 'selectrum)
          (eq selection-system 'consult))
  :hook ((after-init . selectrum-mode))
  :commands (selectrum-exhibit))

(use-package selectrum-prescient
  :if (or (eq selection-system 'selectrum)
          (eq selection-system 'consult))
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package ctrlf
  :if (eq selection-system 'selectrum)
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
  (setq prescient-save-file
        (expand-file-name "prescient-save.el"
                          path-cache-dir))
  (prescient-persist-mode +1))

(use-package consult
  :if (eq selection-system 'consult)
  :bind
  (("M-y" . consult-yank-pop))
  :general
  (leader-def
    "bb" '(consult-buffer :which-key "Switch buffer")
    "pg" '(consult-grep :which-key "Grep the project")
    "ji" '(consult-imenu :which-key "imenu")))

;; (use-package embark
;;   :bind
;;   ("C-M-a" . embark-act))

;; (use-package embark-consult
;;   :if (eq selection-system 'consult)
;;   :after (embark consult)
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :if (eq selection-system 'consult)
  :commands (marginalia-mode
             marginalia-cycle)
  :init
  (marginalia-mode)
  ;; When using Selectrum, ensure that Selectrum is refreshed when
  ;; cycling annotations.
  (advice-add #'marginalia-cycle
              :after
              (lambda ()
                (when (bound-and-true-p selectrum-mode)
                  (selectrum-exhibit))))

  (setq-default marginalia-annotators
                '(marginalia-annotators-heavy
                  marginalia-annotators-light
                  nil)))



(use-package transient
  :defer t
  :init
  (setq
   transient-levels-file (expand-file-name
                          "transient/levels.el"
                          path-cache-dir)
   transient-values-file (expand-file-name
                          "transient/values.el"
                          path-cache-dir)
   transient-history-file (expand-file-name
                           "transient/history.el"
                           path-cache-dir)))



(provide 'init-selection)
;;; init-selection.el ends here
