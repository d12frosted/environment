;;; elfeed-configs.el --- configs file of elfeed configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 11 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(use-package elfeed-web
  :ensure t
  :defer t
  :init
  :config)

(use-package elfeed
  :ensure t
  :defer 1
  :bind ("C-c a e" . elfeed)
  :config
  (setq elfeed-feeds '(("http://www.reddit.com/r/haskelltil/.rss" haskell reddit)
                       ("http://www.reddit.com/r/haskell/.rss" haskell reddit)
                       ("http://www.reddit.com/r/orgmode/.rss" emacs org-mode reddit)
                       ("http://planet.haskell.org/rss20.xml" haskell)
                       ("http://www.reddit.com/r/emacs/.rss" emacs reddit)
                       ("http://nullprogram.com/feed/" emacs)
                       ("http://endlessparentheses.com/atom.xml" emacs))))
