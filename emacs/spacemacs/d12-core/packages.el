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
    evil-nerd-commenter
    ediff
    yasnippet
    exec-path-from-shell
    persp-mode
    (emacs-addiction-mode :location built-in)
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

(defun d12-core/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

(defun d12-core/post-init-persp-mode ()
  (use-package persp-mode
    :init
    (spacemacs|define-custom-layout "@config"
      :binding "c"
      :body
      (find-file-existing (concat d12-path-emacs-home "init.el")))
    (spacemacs|define-custom-layout "@spacemacs"
      :binding "s"
      :body
      (find-file-existing (concat d12-path-spacemacs-distr-home "init.el")))))

(defun d12-core/init-emacs-addiction-mode ()
  (d12-path/load-project "emacs-addiction-mode")
  (require 'emacs-addiction-mode)
  (global-emacs-addiction-mode)
  (setq emacs-addiction-level 'brian))

;;; packages.el ends here
