;;; init-spell.el --- Spell check -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Feb 2021
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
;; Spell check support.
;;
;;; Code:

(advice-add #'flyspell-mode :around #'fun-silent)
(advice-add #'flyspell-prog-mode :around #'fun-silent)

(use-package ispell
  :ensure nil
  :defer t
  :config
  (setq ispell-dictionary "english"
        ispell-program-name (executable-find "aspell"))
  (when (equal (file-name-base ispell-program-name) "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")))

(use-package flyspell
  :ensure nil
  :defer t
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-lazy
  :ensure t
  :after flyspell
  :commands (flyspell-lazy-mode)
  :defines (flyspell-lazy-idle-seconds
            flyspell-lazy-window-idle-seconds)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package flyspell-correct
  :ensure t
  :defer t
  :general
  (leader-def
    "[s" '(flyspell-correct-wrapper
           :which-key "Spelling correction")))

(use-package langtool
  :ensure t
  :defer t
  :init
  (setq langtool-language-tool-server-jar
        (expand-file-name ".nix-profile/share/languagetool-server.jar" path-home-dir)
        langtool-server-user-arguments '("-p" "8082"))
  (setq langtool-language-tool-server-jar nil
        langtool-server-user-arguments nil)
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8081))

(provide 'init-spell)
;;; init-spell.el ends here
