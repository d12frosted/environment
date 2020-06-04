;;; init-base.el --- Base configurations -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Oct 2019
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

(require 'subr-x)
(require 'cl-lib)
(require 'use-package)
(require 'init-path)

;; Introduce myself.
(setq user-full-name "Boris Buliga"
      user-mail-address "boris@d12frosted.io")

(setq-default
 ;; silence advised function warnings
 ad-redefinition-action 'accept

 ;; make `apropos' more useful
 apropos-do-all t
 auto-mode-case-fold nil
 autoload-compute-prefixes nil

 ;; don't ping things that look like domain names
 ffap-machine-p-known 'reject

 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil

 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

 ;; History & backup settings (do not save everything)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil                  ; don't create backup~ files

 ;; byte compilation
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)

 ;; security
 tls-checktrust t
 tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                   ;; compatibility fallbacks
                   "gnutls-cli -p %p %h"
                   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

 ;; clipboard
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
 ;; Use a shared clipboard
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t

 ;; files
 custom-file (concat +path-local-dir "custom.el")
 abbrev-file-name (concat +path-cache-dir "abbrev.el")
 auto-save-list-file-name (concat +path-cache-dir "autosave")
 backup-directory-alist (list (cons "." (concat +path-cache-dir "backup/")))
 pcache-directory (concat +path-cache-dir "pcache/")
 request-storage-directory (concat +path-cache-dir "request")
 server-auth-dir (concat +path-cache-dir "server/")
 shared-game-score-directory (concat +path-etc-dir "shared-game-score/")
 tramp-auto-save-directory (concat +path-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name (concat +path-cache-dir "tramp-persistency.el")
 url-cache-directory (concat +path-cache-dir "url/")
 url-configuration-directory (concat +path-etc-dir "url/")
 gamegrid-user-score-file-directory (concat +path-etc-dir "games/")
 recentf-save-file (concat +path-cache-dir "recentf")

 ;; bookmarks
 bookmark-default-file (concat +path-etc-dir "bookmarks")
 bookmark-save-flag t

 ;; Formatting
 delete-trailing-lines nil  ; `ws-butler' is used for better whitespace handling
 fill-column 80
 sentence-end-double-space nil
 word-wrap t

 ;; Whitespace
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 2
 )

(use-package ws-butler
  :straight (ws-butler
             :type git
             :host github
             :repo "hlissner/ws-butler")
  :diminish
  :commands (ws-butler-global-mode)
  :init
  (ws-butler-global-mode)
  :config
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
                '(special-mode comint-mode term-mode eshell-mode))))

(use-package async
  :commands (async-start))

(use-package server
  :if (display-graphic-p)
  :defer 1
  :commands (server-running-p)
  :config
  (unless (server-running-p)
    (server-start)))

(cl-flet ((always-yes (&rest _) t))
  (defun no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

(provide 'init-base)
;;; init-base.el ends here
