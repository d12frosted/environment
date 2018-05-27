;;; packages.el --- d12-text layer packages file for Spacemacs. -*- lexical-binding: t; -*-
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

(defconst d12-text-packages
  '((fancy-yank :location built-in)
    (ukrainian-input-method
     :location
     (recipe
      :fetcher github
      :repo "d12frosted/emacs-ukrainian-input-method"))
    langtool
    )
  "The list of Lisp packages required by the d12-text layer.")

(defun d12-text/init-fancy-yank ()
  (add-to-load-path-if-exists (concat d12-path-projects-home "personal/fancy-yank"))
  (add-to-load-path-if-exists (concat d12-path-projects-home "fancy-yank"))
  (use-package fancy-yank
    :defer t
    :init
    (defun fancy-yank-extract-github-link (url owner repo type number &rest args)
      (list url
            (concat
             (if (string-equal owner d12-text-github-user)
                 repo
               (concat owner "/" repo))
             (if (string-equal type "milestone")
                 "m"
               "#")
             number)))
    (setq fancy-yank-rules
          `((,d12-text-github-regexp
             . (fancy-yank-extract-regex
                fancy-yank-extract-github-link
                fancy-yank-format-link))
            (,d12-text-http-regexp
             . (fancy-yank-extract-url-title
                fancy-yank-format-link))))
    (mapc (lambda (regexp)
            (map-put fancy-yank-rules
                     regexp
                     (list #'fancy-yank-extract-regex
                           #'fancy-yank-format-link)))
          d12-text-jira-regexps)))

(defun d12-text/init-ukrainian-input-method ()
  (use-package ukrainian-input-method))

(defun d12-core/init-langtool ()
  (use-package langtool
    :defer t
    :init
    (setq
     langtool-language-tool-jar
     "/usr/local/Cellar/languagetool/4.1/libexec/languagetool.jar"
     langtool-language-tool-server-jar
     "/usr/local/Cellar/languagetool/4.1/libexec/languagetool-server.jar")))

;;; packages.el ends here
