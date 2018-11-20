;;; feature/snippets/config.el -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;; URL: https://github.com/d12frosted/environment/emacs
;;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defvar +snippets-dir (expand-file-name "snippets/" nucleus-emacs-dir)
  "Directory where `yasnippet' will search for snippets.")

(def-package! yasnippet
  :commands (yas-minor-mode-on
	     yas-expand
	     yas-expand-snippet
	     yas-lookup-snippet
	     yas-insert-snippet
	     yas-new-snippet
	     yas-visit-snippet-file)
  :init
  ;; Ensure `yas-reload-all' is called as late as possible. Other
  ;; modules could have additional configuration for yasnippet. For
  ;; example, file-templates.
  (add-transient-hook! 'yas-minor-mode-hook (yas-reload-all))

  (add-hook! (text-mode prog-mode conf-mode snippet-mode)
    #'yas-minor-mode-on)

  :config
  (setq yas-verbosity (if nucleus-debug-mode 3 0)
        yas-also-auto-indent-first-line t
	;; Allow nested snippets
        yas-triggers-in-field t)

  ;; Load snippets
  (add-to-list 'yas-snippet-dirs '+snippets-dir nil #'eq)

  ;; Remove GUI dropdown prompt (prefer ivy)
  (delq #'yas-dropdown-prompt yas-prompt-functions)

  ;; Prioritize snippets in `+snippets-dir' over built-in ones if
  ;; there are multiple choices.
  (add-to-list 'yas-prompt-functions #'+snippets-prompt-private nil #'eq)

  ;; Register `def-project-mode!' modes with yasnippet. This enables project
  ;; specific snippet libraries (e.g. for Laravel, React or Jekyll projects).
  ;; TODO: fix me
  ;; (add-hook 'doom-project-hook #'+snippets|enable-project-modes)

  (after! smartparens
    ;; tell smartparens overlays not to interfere with yasnippet keybinds
    (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)))


;; `auto-yasnippet'
(setq aya-persist-snippets-dir (concat nucleus-etc-dir "auto-snippets/"))
