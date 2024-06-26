;;; init-js.el --- JS/TS support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 19 Feb 2021
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
;; JS/TS support.
;;
;; npm i -g typescript-language-server; npm i -g typescript
;;
;;; Code:

(require 'init-elpa)

(use-package js
  :ensure nil
  :defer t
  :init
  (setq js-indent-level 2))

(use-package typescript-mode
  :ensure t
  :defer t
  :hook ((typescript-mode . lsp))
  :init
  (setq typescript-indent-level 2))

(use-package css-mode
  :ensure nil
  :defer t
  :init
  (setq-default css-indent-offset 2))

(use-package vue-mode
  :ensure t
  :defer t)

(provide 'init-js)
;;; init-js.el ends here
