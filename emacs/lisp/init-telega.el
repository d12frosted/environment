;;; init-telega.el --- Telegram inside Em*cs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 17 Feb 2021
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
;; Because Emacs, baby. Chatting in Telegram never was so pleasant and
;; hassle free.
;;
;;; Code:

(use-package telega
  :elpaca (telega
           :host github
           :repo "zevlg/telega.el"
           :branch "releases"
           :files (:defaults "contrib" "etc" "server" "Makefile"))
  :disabled
  :defer t
  :hook ((telega-chat-mode . ui-reset-line-spacing)
         (telega-webpage-mode . ui-reset-line-spacing)
         (telega-chat-mode . editor-disable-electric-pair))
  :general
  (leader-def
    "t" '(:keymap telega-prefix-map))
  :init
  (when (and elpa-bootstrap-p
             env-sys-linux-p)
    (require 'telega-server)
    (require 'lib-nix)
    (let ((exec-path (cons telega-directory exec-path))
          (default-directory telega--lib-directory))
      (unless (executable-find "telega-server")
        (nix-shell-command
         :message-intro "building telega-server..."
         :message-error "Failed to build telega-server"
         :deps '("pkg-config" "tdlib")
         :command (concat
                   (or (executable-find "gmake")
                       "make")
                   " "
                   "LIBS_PREFIX="
                   (expand-file-name telega-server-libs-prefix) " "
                   "INSTALL_PREFIX="
                   (expand-file-name telega-directory) " "
                   "server-reinstall"))))))

(provide 'init-telega)
;;; init-telega.el ends here
