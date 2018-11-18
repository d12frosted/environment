;;; dna.el --- the heart of every cell -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2015-2018 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;; URL: https://github.com/d12frosted/environment/emacs
;;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(nucleus! :feature
       debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       (syntax-checker   ; tasing you for every semicolon you forget
        +childframe)     ; use childframes for error popups (Emacs 26+ only)
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company          ; the ultimate code completion backend
        +auto)           ; as-you-type code completion
       (ivy              ; a search engine for love and life
        +fuzzy)          ; enable fuzzy search backend for ivy

       :ui
       theme
       dashboard
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
       ;;fci               ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :editor
       ;;(format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric          ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       hideshow          ; basic code-folding support
       imenu             ; an imenu sidebar and searchable code index
       term              ; terminals in Emacs
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       ;;docker
       editorconfig      ; let someone else argue about tabs vs spaces
       gist              ; interacting with github gists
       ;;macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit             ; a git porcelain for Emacs
       ;;password-store    ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (haskell +intero) ; a language that's lazier than I am
       latex             ; writing papers in Emacs has never been so fun
       ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       nix               ; I hereby declare "nix geht mehr!"
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present)        ; Emacs for presentations
       plantuml          ; diagrams for confusing people more
       rest              ; Emacs as a REST client
       ruby
       scala             ; java, but good
       (sh +fish)        ; she sells (ba|z|fi)sh shells on the C xor

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;(email +gmail)    ; emacs as an email client
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;(write            ; emacs as a word processor (latex + org + markdown)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme, a custom yasnippet
       ;; library, and additional ex commands for evil-mode. Use it as a
       ;; reference for your own modules.
       (default +bindings +snippets +evil-commands))

(setq nucleus-theme 'leuven
      nucleus-font (font-spec :family "Source Code Pro" :size 12)
      nucleus-big-font (font-spec :family "Source Code Pro" :size 18)
      nucleus-serif-font (font-spec :family "Source Code Pro" :size 12))

(provide 'dna)

;;; dna.el ends here
