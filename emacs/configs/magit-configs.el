;;; magit-configs.el --- configs file of magit configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 02 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

;;; Variables
;; ===========

(defvar git-enable-github-support t
  "If non nil the Github packages and extensions are enabled.")

(defvar git-magit-status-fullscreen t
  "If non nil magit-status buffer is displayed in fullscreen.")

(defvar git-gutter-use-fringe t
  "If non nil the fringe is used to display git-gutter icons.")

;;; Packages configurations
;; =========================

(use-package gist
  :if git-enable-github-support
  :ensure t
  :defer t
  :bind (("C-c v g b" . gist-buffer)
         ("C-c v g B" . gist-buffer-private)
         ("C-c v g l" . gist-list)
         ("C-c v g r" . gist-region)
         ("C-c v g R" . gist-region-private)))

(use-package git-gutter
  :ensure t
  :commands git-gutter-mode
  :diminish git-gutter-mode
  :init
  (init-git-gutter)
  (add-hook 'markdown-mode-hook 'git-gutter-mode)
  (add-hook 'org-mode-hook 'git-gutter-mode)
  (add-hook 'prog-mode-hook 'git-gutter-mode))

(use-package git-gutter-fringe
  :ensure t
  :commands git-gutter-mode
  :bind (("C-c v h s" . git-gutter:stage-hunk)
         ("C-c v h r" . git-gutter:revert-hunk)
         ("C-c v h N" . git-gutter:previous-hunk) ; todo - use p instead of N
         ("C-c v h n" . git-gutter:next-hunk))
  :init
  (setq git-gutter-fr:side 'right-fringe)
  (add-hook 'markdown-mode-hook 'git/load-git-gutter)
  (add-hook 'org-mode-hook 'git/load-git-gutter)
  (add-hook 'prog-mode-hook 'git/load-git-gutter)
  :config
  (setq git-gutter:hide-gutter t)
  ;; Don't need log/message.
  (setq git-gutter:verbosity 0)
  ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
  ;; custom graphics that works nice with half-width fringes
  (fringe-helper-define 'git-gutter-fr:added nil
                        "..X...."
                        "..X...."
                        "XXXXX.."
                        "..X...."
                        "..X...."
                        )
  (fringe-helper-define 'git-gutter-fr:deleted nil
                        "......."
                        "......."
                        "XXXXX.."
                        "......."
                        "......."
                        )
  (fringe-helper-define 'git-gutter-fr:modified nil
                        "..X...."
                        ".XXX..."
                        "XXXXX.."
                        ".XXX..."
                        "..X...."))

(use-package git-messenger
  :ensure t
  :defer t
  :bind ("C-c v m" . git-messenger:popup-message))

(use-package git-timemachine
  :ensure t
  :defer t
  :bind ("C-c v t" . git-timemachine))

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :bind (("C-c v b" . magit-blame-mode)
         ("C-c v l" . magit-log)
         ("C-c v s" . magit-status)
         ("C-c v C" . magit-commit))
  :init
    (setq magit-last-seen-setup-instructions "1.4.0"
          magit-completing-read-function 'magit-ido-completing-read)
  :config
  ;; full screen magit-status
  (when git-magit-status-fullscreen
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    (bind-key "q" 'magit-quit-session magit-status-mode-map))

  (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map))

(use-package magit-gh-pulls
  :if git-enable-github-support
  :ensure t
  :defer t
  :init
  (eval-after-load 'magit
    '(define-key magit-mode-map "#gg" 'd12/load-gh-pulls-mode))
  :config
  (d12|diminish magit-gh-pulls-mode "Github-PR"))

(use-package github-browse-file
  :if git-enable-github-support
  :ensure t
  :defer t
  :bind ("C-c v f b" . github-browse-file))

(use-package git-link
  :if git-enable-github-support
  :ensure t
  :defer t
  :bind (("C-c v f l" . git-link)
         ("C-c v f L" . d12/git-link-copy-url-only)
         ("C-c v f c" . git-link-commit)
         ("C-c v f C" . d12/git-link-commit-copy-url-only))
  :init
  ;; default is to open the generated link
  (setq git-link-open-in-browser t))

(use-package magit-gitflow
  :ensure t
  :commands turn-on-magit-gitflow
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  :config (d12|diminish magit-gitflow-mode "Flow"))
