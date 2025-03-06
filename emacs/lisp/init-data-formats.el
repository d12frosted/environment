;;; init-data-formats.el --- Data Formats -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 06 Mar 2025
;;
;; URL: https://github.com/d12frosted/
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
;;; Code:

(use-package json-snatcher
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t
  :hook
  (yaml-mode . lsp)
  :init
  (setq-default yaml-indent-offset tab-width)
  :config)

(use-package protobuf-mode
  :ensure t
  :defer t)

(provide 'init-data-formats)
;;; init-data-formats.el ends here
