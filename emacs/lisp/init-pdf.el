;;; init-pdf.el --- PDF viewing support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021
;;
;; Author:  <d12frosted@borysb-arch>
;; Maintainer:  <d12frosted@borysb-arch>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
;;   $ cd $HOME/.cache/emacs/packages/28.0/straight/build/pdf-tools/build/server
;;   $ nix-shell -p pkg-config poppler autoconf automake libtool libpng
;;   $ autoreconf -i -f
;;   $ ./autobuild -i $HOME/.cache/emacs/packages/28.0/straight/build/pdf-tools --os nixos
;;
;; See https://github.com/politza/pdf-tools/issues/645#issuecomment-772255271
;; for more information
;;
;;; Code:

(require 'init-elpa)
(require 'init-env)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . auto-revert-mode)
  :commands (pdf-info-close)
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (when (and elpa-bootstrap-p
             (not env-sys-mac-p))
    (require 'pdf-tools)
    (unless (file-exists-p pdf-info-epdfinfo-program)
      (let ((wait-p t)
            (buf (pdf-tools-install 'no-query)))
        (when (bufferp buf)
          (add-hook 'compilation-finish-functions
                    (lambda (buffer _status)
                      (setq wait-p nil)
                      (message
                       (with-current-buffer buffer
                         (buffer-substring (point-min)
                                           (point-max)))))
                    nil t)
          (while wait-p
            (message "building pdf-tools...")
            (sleep-for 1))))))
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
