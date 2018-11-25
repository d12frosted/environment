;;; feature/spellcheck/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Nov 2018
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

(defvar-local +spellcheck-immediately nil
  "If non-nil, spellcheck the current buffer upon starting `flyspell-mode'.

Since spellchecking can be slow in some buffers, so this is
disabled by default. But one can enable it in specific modes or
files. For example,

  (setq-hook! 'TeX-mode-hook +spellcheck-immediately nil)")

;; `ispell'
(setq ispell-dictionary "english"
      ispell-list-command "--list"
      ispell-extr-args '("--dont-tex-check-comments")
      ispell-program-name "aspell")

(after! ispell
  (when (equal (file-name-base ispell-program-name) "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")))

(def-package! flyspell ; built-in
  :defer t
  :init
  (add-hook 'flyspell-mode-hook #'+spellcheck|immediately))

(def-package! flyspell-correct-ivy
  :defer t
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(add-hook! '(text-mode-hook) #'flyspell-mode)
(add-hook! '(prog-mode-hook) #'flyspell-prog-mode)
