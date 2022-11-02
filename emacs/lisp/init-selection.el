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

(elpa-use-package (vertico :files ("*.el" "extensions/*.el"))
  :init
  (vertico-mode))

(elpa-use-package vertico-repeat
  :ensure nil
  :bind ("M-R" . vertico-repeat)
  :hook ((minibuffer-setup . vertico-repeat-save)))

(elpa-use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(elpa-use-package emacs
  :ensure nil
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(elpa-use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(elpa-use-package marginalia
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



(elpa-use-package transient
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
