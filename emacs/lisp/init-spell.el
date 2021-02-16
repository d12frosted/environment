;;; init-spell.el --- Spell check -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021
;;
;; Author:  <d12frosted@borysb-arch>
;; Maintainer:  <d12frosted@borysb-arch>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 16 Feb 2021
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
;; Spell check support.
;;
;;; Code:

(require 'init-elpa)
(require 'init-selection)

(use-package ispell
  :config
  (setq ispell-dictionary "english"
        ispell-program-name "aspell")
  (when (equal (file-name-base ispell-program-name) "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")))

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-lazy
  :after flyspell
  :commands (flyspell-lazy-mode)
  :defines (flyspell-lazy-idle-seconds
            flyspell-lazy-window-idle-seconds)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package flyspell-correct-ivy
  :if (eq selection-system 'ivy)
  :defer t
  :commands (flyspell-correct-ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-correct
  :if (eq selection-system 'selectrum)
  :defer t)

(provide 'init-spell)
;;; init-spell.el ends here
