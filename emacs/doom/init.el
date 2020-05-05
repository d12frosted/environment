;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's
;;      modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       company
       ivy

       :ui
       deft
       doom
       hl-todo
       modeline
       (popup +defaults)
       vc-gutter
       ;; workspaces

       :editor
       file-templates
       fold
       (format +onsave)
       lispy
       multiple-cursors
       ;; rotate-text
       snippets

       :emacs
       dired
       electric
       undo
       vc

       :checkers
       syntax
       spell
       grammar

       :tools
       ;; debugger          ; FIXME stepping through code, to help you add bugs
       docker
       (eval +overlay)
       lookup
       lsp
       (magit +forge)
       make
       ;;pass
       pdf
       rgb

       :lang
       emacs-lisp
       (haskell +lsp +ghcide)
       json
       latex
       ledger
       markdown
       (org +brain
            +journal
            +dragndrop
            +gnuplot
            +roam)
       plantuml
       rest
       scala
       sh
       yaml

       :config
       (default +emacs +smartparens))
