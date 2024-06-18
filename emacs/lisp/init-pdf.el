;;; init-pdf.el --- PDF viewing support -*- lexical-binding: t; -*-
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
;; PDF viewing support.
;;
;; Building pdf-tools on macOS can be daunting. One of the solutions
;; is to use nix package manager (from nix user perspective ,nix is a
;; solution for every problem):
;;
;; $ cd $PACKAGES/straight/build/pdf-tools/build/server
;; $ nix-shell -p pkg-config poppler autoconf automake libtool libpng
;; $ autoreconf -i -f
;; $ ./autobuild -i $PACKAGES/straight/build/pdf-tools --os nixos
;;
;; See https://github.com/politza/pdf-tools/issues/645 for more
;; information.
;;
;;; Code:

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . auto-revert-mode)
  :commands (pdf-info-close
             pdf-tools-install)
  :init
  (setq-default pdf-view-display-size 'fit-page)
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-setup-view-mode))

(defun pdf-setup-view-mode ()
  "Setup `pdf-view-mode'."
  (add-hook 'kill-buffer-hook #'pdf-cleanup-windows nil t)
  (cua-mode 0))

(defun pdf-cleanup-windows ()
  "Kill left-over annotation buffers on document death."
  (when (and (boundp 'pdf-annot-list-document-buffer)
             (buffer-live-p pdf-annot-list-document-buffer))
    (pdf-info-close pdf-annot-list-document-buffer))
  (when (and (boundp 'pdf-annot-list-buffer)
             (buffer-live-p pdf-annot-list-buffer))
    (kill-buffer pdf-annot-list-buffer))
  (let ((contents-buffer (get-buffer "*Contents*")))
    (when (and contents-buffer (buffer-live-p contents-buffer))
      (kill-buffer contents-buffer))))

(provide 'init-pdf)
;;; init-pdf.el ends here
