;;; init-pdf.el --- Emacs is created for reading PDFs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Nov 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :defines (pdf-annot-list-document-buffer
            pdf-annot-list-buffer)
  :commands (pdf-info-close)
  :init
  (setq-default pdf-view-display-size 'fit-page)
  :config
  (unless noninteractive
    (pdf-tools-install))
  (add-hook 'pdf-view-mode-hook #'+pdf|setup-pdf-view-mode))

(defun +pdf|setup-pdf-view-mode ()
  "Setup `pdf-view-mode'."
  (add-hook 'kill-buffer-hook #'+pdf|cleanup-windows nil t)
  (cua-mode 0))

(defun +pdf|cleanup-windows ()
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
