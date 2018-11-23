;;; completion/ivy/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

(defvar +ivy-task-tags
  '(("TODO"  . warning)
    ("FIXME" . error))
  "An alist of tags for `+ivy/tasks' to include in its search,
whose CDR is the face to render it with.")

(defvar +ivy-project-search-engines '(rg ag pt)
  "What search tools for `+ivy/project-search' (and
`+ivy-file-search' when no ENGINE is specified) to try, and in
what order.

To disable a particular tool, remove it from this list. To
prioritize a tool over others, move it to the front of the
list. Later duplicates in this list are silently ignored.

If you want to already use git-grep or grep, set this to nil.")

(defmacro +ivy-do-action! (action)
  "Returns an interactive lambda that sets the current ivy action
and immediately runs it on the current candidate (ending the ivy
session)."
  `(lambda ()
     (interactive)
     (ivy-set-action ,action)
     (setq ivy-exit 'done)
     (exit-minibuffer)))

(def-package! ivy
  :defer 1
  :after-call pre-command-hook
  :config
  (setq projectile-completion-system 'ivy
	ivy-height 15
	ivy-wrap t
	ivy-fixed-height-minibuffer t
	ivy-format-function #'ivy-format-function-line
	;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
	;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
	;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
	;; don't quit minibuffer on delete-error
        ivy-on-del-error-function nil)

  (define-key! 'global
    [remap switch-to-buffer]       #'ivy-switch-buffer
    [remap persp-switch-to-buffer] #'+ivy/switch-workspace-buffer
    [remap imenu-anywhere]         #'ivy-imenu-anywhere)

  (ivy-mode +1))

(def-package! counsel
  :commands counsel-describe-face
  :init
  (define-key! 'global
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap describe-face]            #'counsel-faces
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap imenu]                    #'counsel-imenu
    [remap recentf-open-files]       #'counsel-recentf
    [remap org-capture]              #'counsel-org-capture
    [remap swiper]                   #'counsel-grep-or-swiper)
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        ;; Add smart-casing and compressed archive searching (-zS) to
        ;; default command arguments:
        counsel-rg-base-command "rg -zS --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
        counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s"))

(def-package! counsel-projectile
  :commands (counsel-projectile-find-file
	     counsel-projectile-find-dir
	     counsel-projectile-switch-to-buffer
             counsel-projectile-grep
	     counsel-projectile-ag
	     counsel-projectile-switch-project)
  :init
  (define-key! 'global
    [remap projectile-find-file]        #'+ivy/projectile-find-file
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))
