;;; init-vcs.el --- VCS configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
(require 'lib-vcs)
(require 'config-path)

(setq-default vc-follow-symlinks t)

;; (remove-hook 'find-file-hook #'vc-refrqesh-state)
;; (hook-with-delay 'find-file-hook 1 #'vc-refresh-state)

(use-package magit
  :ensure t
  :defer t
  :defines (magit-status-mode-map
            magit-revision-show-gravatars
            magit-display-buffer-function
            magit-diff-refine-hunk)
  :commands (magit-display-buffer-same-window-except-diff-v1
             magit-stage-file
             magit-unstage-file)
  :general
  (leader-def
    "g" '(nil :which-key "git...")
    "gS" '(magit-stage-file :which-key "stage file")
    "gU" '(magit-unstage-file :which-key "unstage file")
    "g[" '(git-gutter:previous-hunk :which-key "previous hunk")
    "g]" '(git-gutter:next-hunk :which-key "next hunk")
    "gd" '(magit-dispatch :which-key "dispatch")
    "gf" '(magit-find-file :which-key "find-file")
    "gg" '(magit-status :which-key "status")
    "gi" '(magit-init :which-key "initialize repo")
    "gt" '(git-timemachine-toggle :which-key "time machine"))
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

(use-package closql
  :ensure t
  :defer t)

(use-package ghub
  :ensure t
  :defer t)

(use-package forge
  :ensure t
  :commands forge-create-pullreq forge-create-issue
  :init
  (setq-default forge-database-file
                (expand-file-name "forge/forge-database.sqlite"
                                  path-etc-dir)))

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package ediff-wind
  :ensure nil
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(use-package diff-hl
  :ensure t
  :defer t
  :hook ((prog-mode . turn-on-diff-hl-mode)
         (text-mode . turn-on-diff-hl-mode)))

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
      (mapc #'vcs--kill-buffer (magit-mode-get-buffers)))))

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

(provide 'init-vcs)
;;; init-vcs.el ends here
