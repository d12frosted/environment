;;; funcs.el --- d12frosted-haskell Layer funcs File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defun shm-repl-tab ()
  "TAB completion or jumping."
  (interactive)
  (unless (shm/jump-to-slot)
    (call-interactively 'haskell-interactive-mode-tab)))

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (unless (= (line-beginning-position)
             (line-end-position))
    (shm/backward-paragraph))
  (unless (= (line-beginning-position)
             (line-end-position))
    (save-excursion (insert "\n")))
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defvar haskell-process-use-ghci nil)

(defun haskell-process-cabal-build-and-restart ()
  "Build and restart the Cabal project."
  (interactive)
  (cond
   (haskell-process-use-ghci
    (when (buffer-file-name)
      (save-buffer))
    ;; Reload main module where `main' function is
    (haskell-process-reload-devel-main))
   (t
    (haskell-process-cabal-build)
    (haskell-process-queue-without-filters
     (haskell-process)
     (format ":!cd %s && scripts/restart\n" (haskell-session-cabal-dir (haskell-session)))))
   (t (turbo-devel-reload))))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (cond
     ;; Use grep
     (nil (let ((buffer
                 (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                    (haskell-session-current-dir (haskell-session))
                                    sym))))
            (with-current-buffer buffer
              (rename-buffer "*who-calls*")
              (switch-to-buffer-other-window buffer))))
     ;; Use ag
     (t (ag-files sym
                  "\\.hs$"
                  (haskell-session-current-dir (haskell-session)))))))

(defun haskell-auto-insert-module-template ()
  "Insert a module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
		(point-max))
	     (buffer-file-name))
    (insert
     "{-# LANGUAGE NoImplicitPrelude #-}\n"
     "{-# LANGUAGE OverloadedStrings #-}\n\n"
     "module "
     )
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
	  (progn (insert "Main")
		 (shm-evaporate (- (point) 5)
				(point)))
	(insert name)))
    (insert " where\n\n"
	    "import BasicPrelude\n\n")
    (goto-char (point-min))
    (forward-char 4)))

(defun shm-contextual-space ()
  "Do contextual space first, and run shm/space if no change in
the cursor position happened."
  (interactive)
  (if god-local-mode
      (call-interactively 'god-mode-self-insert)
    (if (looking-back "import")
        (call-interactively 'haskell-mode-contextual-space)
      (progn
        (let ((ident (haskell-ident-at-point)))
          (when ident
            (and interactive-haskell-mode
                 (haskell-process-do-try-type ident))))
        (call-interactively 'shm/space)))))

(defun shm/insert-putstrln ()
  "Insert a putStrLn."
  (interactive)
  (let ((name
         (save-excursion
           (goto-char (car (shm-decl-points)))
           (buffer-substring-no-properties
            (point)
            (1- (search-forward " "))))))
    (insert
     (format "putStrLn \"%s:%s:%d\""
             (file-name-nondirectory (buffer-file-name))
             name
             (line-number-at-pos)))))

(defun haskell-process-all-types ()
      "List all types in a grep-mode buffer."
      (interactive)
      (let ((session (haskell-session)))
        (switch-to-buffer (get-buffer-create (format "*%s:all-types*"
                                                     (haskell-session-name (haskell-session)))))
        (setq haskell-session session)
        (cd (haskell-session-current-dir session))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((haskell-process-log nil))
            (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
          (unless (eq major-mode  'compilation-mode)
            (compilation-mode)
            (setq compilation-error-regexp-alist
                  haskell-compilation-error-regexp-alist)))))
