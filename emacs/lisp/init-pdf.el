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
;; PDF viewing support.
;;
;;; Code:

(require 'init-elpa)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :defines (pdf-annot-list-document-buffer
            pdf-annot-list-buffer)
  :commands (pdf-info-close
             pdf-tools-install)
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (when elpa-bootstrap-p
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
          (sleep-for 1)))))
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-setup-view-mode))

(defun pdf-setup-view-mode ()
  "Setup `pdf-view-mode'."
  (add-hook 'kill-buffer-hook #'pdf-cleanup-windows nil t)
  (cua-mode 0))

(defun pdf-cleanup-windows ()
  "Kill left-over annotation buffers on document death."
  (when (buffer-live-p pdf-annot-list-document-buffer)
    (pdf-info-close pdf-annot-list-document-buffer))
  (when (buffer-live-p pdf-annot-list-buffer)
    (kill-buffer pdf-annot-list-buffer))
  (let ((contents-buffer (get-buffer "*Contents*")))
    (when (and contents-buffer (buffer-live-p contents-buffer))
      (kill-buffer contents-buffer))))

(provide 'init-pdf)
;;; init-pdf.el ends here
