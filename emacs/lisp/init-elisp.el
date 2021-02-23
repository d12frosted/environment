;;; init-elisp.el --- Main language of the neighborhood -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 08 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
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
;; While Emacs Lisp is supported out of the box, there are still
;; things to improve.
;;
;;; Code:

(use-package lispy
  :diminish
  :defines (lispy-mode-map)
  :hook ((emacs-lisp-mode . lispy-mode))
  :bind (:map lispy-mode-map
              ("C-a" . beginning-of-line)))

(use-package eldoc
  :straight nil
  :diminish eldoc-mode)

(use-package flycheck-eldev
  :after flycheck)

(use-package form-feed
  :hook ((emacs-lisp-mode . form-feed-mode)))

(use-package emacsql
  :defer t
  :hook ((emacs-lisp-mode . emacsql-fix-vector-indentation)))

(provide 'init-elisp)
;;; init-elisp.el ends here
