;;; config/default/packages.el -*- lexical-binding: t; -*-

(when IS-MAC
  (package! exec-path-from-shell)
  (package! osx-clipboard)
  (package! ns-auto-titlebar))
