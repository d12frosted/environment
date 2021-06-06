;;; init-vcs.el --- VCS configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 07 Feb 2021
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
;; Git is wonderful. Especially when you have `magit'.
;;
;;; Code:

(require 'lib-hook)
(require 'config-path)
(require 'init-elpa)

(setq-default vc-follow-symlinks t)

(remove-hook 'find-file-hook #'vc-refresh-state)

(hook-with-delay 'find-file-hook 1 #'vc-refresh-state)

(use-package magit
  :defer t
  :defines (magit-status-mode-map
            magit-revision-show-gravatars
            magit-display-buffer-function
            magit-diff-refine-hunk)
  :commands (magit-display-buffer-same-window-except-diff-v1
             magit-stage-file
             magit-unstage-file)
  :init
  (setq-default magit-git-executable (executable-find "git"))
  :config
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map
    [remap magit-mode-bury-buffer]
    #'vcs-quit)

  (setq magit-revision-show-gravatars
        '("^Author:     " . "^Commit:     ")
        magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        ;; show word-granularity on selected hunk
        magit-diff-refine-hunk t))

(use-package forge
  :commands forge-create-pullreq forge-create-issue
  :init
  (setq-default forge-database-file
                (expand-file-name "forge/forge-database.sqlite"
                                  path-etc-dir)))

(use-package git-timemachine
  :defer t)

(use-package git-gutter-fringe
  :defer t
  :defines (git-gutter-mode)
  :commands (git-gutter-fr:init
             git-gutter-fr:view-diff-infos
             git-gutter-fr:clear
             git-gutter:view-diff-infos
             git-gutter:clear-diff-infos
             git-gutter:update-all-windows
             git-gutter-mode
             git-gutter)
  :init
  ;; (hook-with-delay 'text-mode-hook 1 #'vcs-gutter-maybe)
  ;; (hook-with-delay 'prog-mode-hook 1 #'vcs-gutter-maybe)
  ;; (hook-with-delay 'conf-mode-hook 1 #'vcs-gutter-maybe)
  ;; (add-hook 'after-save-hook #'vcs-gutter-maybe)
  :config
  ;; Update git-gutter on focus (in case I was using git externally)
  (add-function :after
                after-focus-change-function
                #'git-gutter:update-all-windows)

  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'vcs-gutter-update)
  (advice-add #'magit-unstage-file :after #'vcs-gutter-update)

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(use-package ediff-wind
  :straight nil
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(defun vcs-quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'.

And don't forget to refresh version control in all buffers of
current workspace."
  (interactive)
  (quit-window)
  (unless (cdr
           (delq nil
                 (mapcar (lambda (win)
                           (with-selected-window win
                             (eq major-mode 'magit-status-mode)))
                         (window-list))))
    (when (fboundp 'magit-mode-get-buffers)
      (mapc #'vcs--kill-buffer (magit-mode-get-buffers)))
    ;; (async-start
    ;;  (lambda ()
    ;;    (dolist (buffer (buffer-list))
    ;;      (with-current-buffer buffer
    ;;        (vc-refresh-state)
    ;;        (when (fboundp 'vcs-gutter-update)
    ;;          (vcs-gutter-update)))))
    ;;  'ignore)
    ))

(defun vcs--kill-buffer (buffer)
  "Gracefully kill `magit' BUFFER.

If any alive process is related to this BUFFER, wait for 5
seconds before nuking BUFFER and the process. If it's dead -
don't wait at all."
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (let ((process (get-buffer-process buffer)))
      (if (not (processp process))
          (kill-buffer buffer)
        (with-current-buffer buffer
          (if (process-live-p process)
              (run-with-timer 5 nil #'vcs--kill-buffer buffer)
            (kill-process process)
            (kill-buffer buffer)))))))

(defun vcs-gutter-update (&rest _)
  "Refresh `git-gutter'."
  (when git-gutter-mode
    (ignore (git-gutter))))

(defun vcs-gutter-maybe ()
  "Enable `git-gutter' in non-remote buffers."
  (when (and buffer-file-name
             (vc-backend buffer-file-name)
             (not (file-remote-p buffer-file-name)))
    (if (display-graphic-p)
        (progn
          (require 'git-gutter-fringe)
          (setq-local git-gutter:init-function
                      #'git-gutter-fr:init)
          (setq-local git-gutter:view-diff-function
                      #'git-gutter-fr:view-diff-infos)
          (setq-local git-gutter:clear-function
                      #'git-gutter-fr:clear)
          (setq-local git-gutter:window-width -1))
      (setq-local git-gutter:init-function 'nil)
      (setq-local git-gutter:view-diff-function
                  #'git-gutter:view-diff-infos)
      (setq-local git-gutter:clear-function
                  #'git-gutter:clear-diff-infos)
      (setq-local git-gutter:window-width 1))
    (git-gutter-mode +1)))

(provide 'init-vcs)
;;; init-vcs.el ends here
