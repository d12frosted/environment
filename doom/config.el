;;; doom/config.el -*- lexical-binding: t; -*-

;; https://github.com/fniessen/emacs-leuven-theme
(def-package! leuven-theme)

;; Disable line numbers
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)
