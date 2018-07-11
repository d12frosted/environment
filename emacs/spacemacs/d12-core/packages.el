;;; packages.el --- d12-core layer packages file for Spacemacs. -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst d12-core-packages
  '(
    (d12-macos-reveal :location (recipe :fetcher local)
                      :toggle (spacemacs/system-is-mac))
    (d12-dir-settings :location (recipe :fetcher local))
    ace-window
    projectile
    evil-nerd-commenter
    ediff
    yasnippet
    )
  "The list of Lisp packages required by the d12-core layer.")

(defun d12-core/init-d12-macos-reveal ()
  (use-package d12-macos-reveal
    :commands (d12-macos-reveal/file d12-macos-reveal/project-root)
    :init
    (spacemacs/declare-prefix "or" "reveal")
    (spacemacs/set-leader-keys
      "orf" #'d12-macos-reveal/file
      "orr" #'d12-macos-reveal/project-root)))

(defun d12-core/init-d12-dir-settings ()
  (use-package d12-dir-settings
    :defer t
    :init
    (add-hook 'find-file-hook #'d12-dir-settings/load)))

(defun d12-core/post-init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (bind-key "M-`" 'ace-window)
    (bind-key "M-p" 'ace-window)))

(defun d12-core/pre-init-projectile ()
  (use-package projectile
    :defer t
    :init
    ;; Disable projectile caching as it slows down file switching in huge
    ;; projects. TODO: investigate if it's still the case.
    (setq projectile-enable-caching nil)))

(defun d12-core/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :commands (evilnc-comment-or-uncomment-lines
               evilnc-quick-comment-or-uncomment-to-the-line
               evilnc-copy-and-comment-lines
               evilnc-comment-or-uncomment-paragraphs)
    :init
    (d12-key-bind "M-;" #'evilnc-comment-or-uncomment-lines)
    (d12-key-bind "C-M-;" #'evilnc-comment-or-uncomment-paragraphs)
    ;; (d12-key-bind "C-c c" #'evilnc-copy-and-comment-lines)
    ))

(defun d12-core/post-init-ediff ()
  (use-package ediff
    :init
    (defun ediff-copy-A-and-B-to-C ()
      (interactive)
      (ediff-copy-diff ediff-current-difference nil 'C nil
                       (concat
                        (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                        (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
    (defun add-d-to-ediff-mode-map ()
      (define-key ediff-mode-map "d" #'ediff-copy-A-and-B-to-C))
    (add-hook 'ediff-keymap-setup-hook #'add-d-to-ediff-mode-map)
    :config))

(defun d12-core/post-init-yasnippet ()
  (use-package yasnippet
    :defer t
    :init
    (add-to-list 'yas-snippet-dirs (concat d12-path-emacs-home "snippets"))
    :config
    (mapc #'d12-path/make-directory-safe (yas-snippet-dirs))
    ))

;;; packages.el ends here
