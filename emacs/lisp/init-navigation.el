;;; init-navigation.el --- navigation related configurations -*- lexical-binding: t; -*-
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

(use-package avy
  :defer t
  :general
  (+leader-def
      "jL" '(goto-line :which-key "Line (by number)")
      "jl" '(avy-goto-line :which-key "Line (avy)")
      "jj" '(avy-goto-char :which-key "Char")
      "jJ" '(avy-goto-char-timer :which-key "Chars")
      "jw" '(avy-goto-word-0 :which-key "Word")))

(use-package ace-link
  :defer t
  :general
  (+leader-def
    "jb" '(ace-link :which-key "Button or link")))

(provide 'init-navigation)
;;; init-navigation.el ends here
