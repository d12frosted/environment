;;; tools/pdf/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Jul 2019
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(def-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (unless noninteractive
    (pdf-tools-install))

  (defun +pdf|cleanup-windows ()
    "Kill left-over annotation buffers when the document is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))
  (add-hook! 'pdf-view-mode-hook
    (add-hook 'kill-buffer-hook #'+pdf|cleanup-windows nil t))

  (setq-default pdf-view-display-size 'fit-page)
  ;; Turn off cua so copy works
  (add-hook! 'pdf-view-mode-hook (cua-mode 0))
  ;; Handle PDF-tools related popups better
  ;; (set-popup-rule! "^\\*Outline*" :side 'right :size 40 :select nil)
  ;; The next rules are not needed, they are defined in modules/ui/popups/+hacks.el
  ;; (set-popup-rule! "\\*Contents\\*" :side 'right :size 40)
  ;; (set-popup-rule! "* annots\\*$" :side 'left :size 40 :select nil)
  )
