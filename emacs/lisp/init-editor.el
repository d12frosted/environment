;;; init-editor.el --- some things editor -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 04 Jun 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-package)
(require '+vcs-url)
(require '+string)

(use-package ukrainian-input-method
  :straight (ukrainian-input-method
             :type git
             :host github
             :repo "d12frosted/emacs-ukrainian-input-method"))

(use-package move-text
  :commands (move-text-down
             move-text-up))

(use-package fancy-yank
  :straight (fancy-yank
             :type git
             :host github
             :repo "d12frosted/fancy-yank")
  :commands (fancy-yank)
  :init
  (setq-default
   fancy-yank-rules
   (list
    (cons +vcs-url-github-issue-regexp
          '(fancy-yank-extract-regex
            (lambda (url owner repo type number &rest args)
              (list url
                    (+vcs-url-format-github-issue owner repo type number)))
            fancy-yank-format-link))
    (cons +vcs-url-github-project-regexp
          '(fancy-yank-extract-regex
            (lambda (url owner repo &rest args)
              (list url
                    (+vcs-url-format-github-project owner repo)))
            fancy-yank-format-link))
    (cons "\\(https?://hackage.haskell.org/package/\\([-[:alnum:]]+\\).*\\)"
          '(fancy-yank-extract-regex
            (lambda (url package &rest args)
              (list
               url
               package))
            fancy-yank-format-link))
    (cons +string-http-url-regexp
          '(fancy-yank-extract-regex
            (lambda (url &rest args)
              (list
               url
               (or (ignore-errors (url-domain (url-generic-parse-url url)))
                   (read-string "Description: "))))
            fancy-yank-format-link)))))

(use-package apheleia
  :straight (apheleia
             :host github
             :repo "raxod502/apheleia")
  :commands (apheleia-global-mode)
  :defer t
  :init
  (apheleia-global-mode +1))

(provide 'init-editor)
;;; init-editor.el ends here
