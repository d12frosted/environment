;;; packages.el --- d12frosted-visual layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
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
    (d12-pretty-greek :location local)
    all-the-icons
    spaceline-all-the-icons))

(defun d12frosted-visual/init-beacon ()
  (use-package beacon
    :init
    (beacon-mode 1)))

;; (defun d12frosted-visual/post-init-spaceline ()
;;   (use-package spaceline-config
;;     :init
;;     (setq powerline-default-separator 'utf-8)
;;     :config
;;     (require 'cl)
;;     (defvar d12-state-cursors '((emacs "SkyBlue2" box)
;;                                 (emacs-input "chartreuse3" box)
;;                                 (god "DarkGoldenrod2" box)
;;                                 (god-input "plum3" box))
;;       "Colors assigned to several states with cursor definitions.")

;;     (cl-loop for (state color cursor) in d12-state-cursors
;;              do
;;              (eval `(defface ,(intern (format "d12-spaceline-%S-face" state))
;;                       `((t (:background ,color
;;                                         :foreground ,(face-background 'mode-line)
;;                                         :box ,(face-attribute 'mode-line :box)
;;                                         :inherit 'mode-line)))
;;                       (format "%s state face." state)
;;                       :group 'd12frosted))
;;              (set (intern (format "d12-%s-state-cursor" state))
;;                   (list (when dotspacemacs-colorize-cursor-according-to-state color)
;;                         cursor)))

;;     (defun d12//get-state ()
;;       (cond
;;        ((and (bound-and-true-p current-input-method) (bound-and-true-p god-local-mode)) 'god-input)
;;        ((bound-and-true-p current-input-method) 'emacs-input)
;;        ((bound-and-true-p god-local-mode) 'god)
;;        (t 'emacs)))
;;     (defun d12//get-state-face ()
;;       (let ((state (d12//get-state)))
;;         (intern (format "d12-spaceline-%S-face" state))))
;;     (setq spaceline-highlight-face-func 'd12//get-state-face)

;;     (spaceline-toggle-minor-modes-off)
;;     (spaceline-define-segment major-mode
;;       "The name of the major mode."
;;       (d12-visual/powerline-major-mode))))

(defun d12frosted-visual/init-d12-pretty-greek ()
  (use-package d12-pretty-greek
    :init
    (add-hook 'prog-mode-hook #'d12-pretty-greek)))

(defun d12frosted-visual/init-all-the-icons ()
  (use-package all-the-icons))

(defun d12frosted-visual/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :after spaceline
    :init
    (setq spaceline-all-the-icons-separator-type 'none)
    (setq spaceline-all-the-icons-icon-set-flycheck-slim 'dots)
    (setq spaceline-all-the-icons-icon-set-git-ahead 'commit)
    ;; (setq spaceline-all-the-icons-icon-set-window-numbering 'square)
    (setq spaceline-all-the-icons-flycheck-alternate t)
    (setq spaceline-all-the-icons-highlight-file-name t)
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-anzu)
    (spaceline-all-the-icons--setup-package-updates)
    (spaceline-all-the-icons--setup-paradox)
    (spaceline-all-the-icons--setup-neotree)
    (spaceline-toggle-all-the-icons-buffer-path-off)
    (spaceline-toggle-all-the-icons-fullscreen-on)
    (spaceline-toggle-all-the-icons-flycheck-status-on)
    (spaceline-toggle-all-the-icons-vc-status-on)
    (spaceline-toggle-all-the-icons-git-status-on)
    (spaceline-toggle-all-the-icons-vc-icon-on)
    (spaceline-toggle-all-the-icons-mode-icon-on)
    (spaceline-toggle-all-the-icons-package-updates-on)
    (spaceline-toggle-all-the-icons-window-number-off)
    (spaceline-toggle-all-the-icons-projectile-off)

    ;; (spaceline-toggle-all-the-icons-text-scale-on)
    (spaceline-toggle-all-the-icons-region-info-on)
    :config (spaceline-all-the-icons-theme)))

;;; packages.el ends here
