;;; init-spellcheck.el --- spell checking -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
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

;; `ispell'
(setq ispell-dictionary "english"
      ispell-list-command "--list"
      ispell-extr-args '("--dont-tex-check-comments")
      ispell-program-name "aspell")

(with-eval-after-load 'ispell
  (when (equal (file-name-base ispell-program-name) "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")))

(use-package flyspell-correct-ivy
  :general
  (+leader-def
    "[s" '(flyspell-correct-wrapper :which-key "Spelling correction"))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(provide 'init-spellcheck)
;;; init-spellcheck.el ends here
