;;; packages.el --- d12frosted Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defvar d12frosted-packages
  '(elfeed
    elfeed-web
    2048-game)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar d12frosted-excluded-packages '()
  "List of packages to exclude.")

(defun d12frosted/init-elfeed-web ()
  "Initialize elfeed-web package"
  (use-package elfeed-web
    :defer t
    :init
    :config))

(defun d12frosted/init-elfeed ()
  "Initialize elfeed package."
  (use-package elfeed
    :defer 1
    :init
    :config
    (evil-leader/set-key "oe" 'elfeed)
    (setq elfeed-feeds '(("http://www.reddit.com/r/haskelltil/.rss" haskell reddit)
                         ("http://www.reddit.com/r/haskell/.rss" haskell reddit)
                         ("http://www.reddit.com/r/orgmode/.rss" emacs org-mode reddit)
                         ("http://planet.haskell.org/rss20.xml" haskell)
                         ("http://www.reddit.com/r/emacs/.rss" emacs reddit)
                         ("http://nullprogram.com/feed/" emacs)))))

(defun d12frosted/init-2048-game ()
  "Initialize 2048-game package."
  (use-package 2048-game
    :defer t
    :init
    :config))
