;;; packages.el --- d12frosted-visual layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-visual-packages
  '(
    beacon
    spaceline
    (d12-pretty-greek :location local)))

(defun d12frosted-visual/init-beacon ()
  (use-package beacon
    :init
    (beacon-mode 1)))

(defun d12frosted-visual/post-init-spaceline ()
  (use-package spaceline-config
    :init
    (setq powerline-default-separator 'utf-8)
    :config
    (require 'cl)
    (defvar d12-state-cursors '((emacs "SkyBlue2" box)
                                (emacs-input "chartreuse3" box)
                                (god "DarkGoldenrod2" box)
                                (god-input "plum3" box))
          "Colors assigned to several states with cursor definitions.")

    (cl-loop for (state color cursor) in d12-state-cursors
             do
             (eval `(defface ,(intern (format "d12-spaceline-%S-face" state))
                      `((t (:background ,color
                                        :foreground ,(face-background 'mode-line)
                                        :box ,(face-attribute 'mode-line :box)
                                        :inherit 'mode-line)))
                      (format "%s state face." state)
                      :group 'd12frosted))
             (set (intern (format "d12-%s-state-cursor" state))
                  (list (when dotspacemacs-colorize-cursor-according-to-state color)
                        cursor)))

    (defun d12//get-state ()
      (cond
       ((and (bound-and-true-p current-input-method) (bound-and-true-p god-local-mode)) 'god-input)
       ((bound-and-true-p current-input-method) 'emacs-input)
       ((bound-and-true-p god-local-mode) 'god)
       (t 'emacs)))
    (defun d12//get-state-face ()
      (let ((state (d12//get-state)))
        (intern (format "d12-spaceline-%S-face" state))))
    (setq spaceline-highlight-face-func 'd12//get-state-face)))

(defun d12frosted-visual/init-d12-pretty-greek ()
  (use-package d12-pretty-greek
    :init
    (add-hook 'prog-mode-hook #'d12-pretty-greek)))

;;; packages.el ends here
