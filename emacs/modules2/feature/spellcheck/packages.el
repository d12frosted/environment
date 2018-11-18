;; -*- no-byte-compile: t; -*-
;;; feature/spellcheck/packages.el

(package! flyspell-correct)

(cond ((featurep! :completion ivy)
       (package! flyspell-correct-ivy)))
