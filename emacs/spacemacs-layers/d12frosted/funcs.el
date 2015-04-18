;;; funcs.el --- d12frosted Layer funcs File for Spacemacs
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; general functions

(defun d12frosted/concat-path (dir f)
  "Append F to DIR with respect of trailing slash in DIR."
  (if (s-ends-with? "/" dir)
      (s-append f dir)
    (s-concat dir "/" f)))

(defun d12frosted/directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (-filter (lambda (f) (file-directory-p f))
           (-map (lambda (f) (d12frosted/concat-path dir f))
                 (-filter (lambda (f) (not (-contains? '("." "..") f))) (directory-files dir))))
  )

(defun d12frosted/directory-dirs-r (dir)
  "Find all directories in DIR (recursive)."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (d12frosted/directory-dirs-r file))
                               dirs))))))
    dirs))

(defun fold (f x list)
  "Recursively applies (F i j) to LIST starting with X.
For example, (fold F X '(1 2 3)) computes (F (F (F X 1) 2) 3)."
  (let ((li list) (x2 x))
    (while li
      (setq x2 (funcall f x2 (pop li))))
    x2))

;;; org-mode functions

(defun gtd ()
   (interactive)
   (find-file (d12frosted/concat-path d12frosted/org-home-path "gtd/gtd.org")))

(defun d12frosted/org-files-in-folder (folder)
  (directory-files folder t ".*\.org$"))

(defun d12frosted/org-insert-block-template ()
  "Insert block template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("h" . "HTML")
                      ("q" . "QUOTE")
                      ("c" . "CENTER")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "\n#+END_" choice)
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "\n#+END_" choice))))))))))

(defun d12frosted/org-new-file-template ()
  "Create template for new org file."
  (let ((option-key-width 16)
        (bname (buffer-name))
        (title (if (boundp 'bname)
                  (if (s-suffix? ".org" bname)
                      (substring bname 0 -4)
                    bname)
                "Yet another org file"))
        (author "Boris Buliga <d12frosted@icloud.com>")
        (email "d12frosted@icloud.com")
        (date (format-time-string "%Y-%m-%d")))
    (s-join "\n" (list (d12frosted/org-option option-key-width "TITLE" title)
                       (d12frosted/org-option option-key-width "AUTHOR" author)
                       (d12frosted/org-option option-key-width "EMAIL" email)
                       (d12frosted/org-option option-key-width "DATE" date)
                       (d12frosted/org-option option-key-width "STARTUP" "showeverything")
                       (d12frosted/org-option option-key-width "OPTIONS" "toc:nil")))))

(defun d12frosted/org-option (width key value)
  "Create an option string for org file."
  (s-append value (s-pad-right width " " (s-concat "#+" key ":"))))

(defun d12frosted/org-auto-insert-template ()
  "Insert template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert (d12frosted/org-new-file-template))
    (goto-char (point-max))))

;;; omnisharp functions

(defun csharp-hs-forward-sexp (&optional arg)
    "Stolen from emacswiki"
    (message "csharp-hs-forward-sexp, (arg %d) (point %d)..."
             (if (numberp arg) arg -1)
             (point))

    (let ((nestlevel 0)
          (mark1 (point))
          (done nil))

      (if (and arg (< arg 0))
          (message "negative arg (%d) is not supported..." arg)

        ;; else, we have a positive argument, hence move forward.
        ;; simple case is just move forward one brace
        (if (looking-at "{")
            (forward-sexp arg)

          ;; The more complex case is dealing with a "region/endregion" block.
          ;; We have to deal with nested regions!
          (and
           (while (not done)
             (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                                (point-max) 'move)
             (cond

              ;; do nothing if at end of buffer
              ((eobp))

              ((and
                (match-beginning 1)
                ;; if the match is longer than 6 chars, we know it is "endregion"
                (if (> (- (match-end 1) (match-beginning 1)) 6)
                    (setq nestlevel (1- nestlevel))
                  (setq nestlevel (1+ nestlevel))))))

             (setq done (not (and (> nestlevel 0) (not (eobp))))))

           (if (= nest 0)
               (goto-char (match-end 2))))))))

(defun d12frosted/omnisharp-go-to-definition-at-center ()
  (interactive)
  (omnisharp-go-to-definition)
  (recenter))

(defun d12frosted/omnisharp-on-load-fn ()
  "Function that should be called when omnisharp mode is enabled."

  (spacemacs|diminish omnisharp-mode " ⓞ" " o")

  (setq indent-tabs-mode t
            c-default-style "k&r"
            c-basic-offset 2
            hs-isearch-open t)

  (c-set-offset 'case-label '+)
  (c-set-offset 'cpp-macro 'csharp-lineup-if-and-region)

  (local-set-key (kbd "C-c <") 'hs-hide-block)
  (local-set-key (kbd "C-c >") 'hs-show-block)

  (local-unset-key (kbd "{"))
  (local-unset-key (kbd "/"))
  (local-unset-key (kbd "C-c C-d"))

  (local-set-key (kbd "M-.") 'omnisharp-auto-complete)

  ;; todo - menu integration!
  (local-set-key (kbd "C-c o y") 'd12frosted/omnisharp-go-to-definition-at-center)
  (local-set-key (kbd "C-c o u") 'omnisharp-find-usages)
  (local-set-key (kbd "C-c o r") 'omnisharp-rename)
  (local-set-key (kbd "C-c o i") 'omnisharp-current-type-information)
  (local-set-key (kbd "C-c o f") 'omnisharp-navigate-to-solution-file)
  (local-set-key (kbd "C-c o g") 'omnisharp-navigate-to-current-file-member)
  (local-set-key (kbd "C-c o o") 'omnisharp-auto-complete-overrides))

;;; text manipulations

(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun copy-line-or-region ()
  "Copy current line (with newline character) or region. When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (line-or-region-point-min)
                  (line-or-region-point-max))
  (message "copied"))

(defun cut-line-or-region ()
  "Cut current line or region. When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-region (line-or-region-point-min)
               (line-or-region-point-max))
  (message "cut"))

(defun duplicate-line-or-region ()
  "Duplicates current line or region. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (kill-ring-save (line-or-region-point-min)
                  (line-or-region-point-max))
  (move-beginning-of-line 1)
  (yank)
  (message "duplicated"))

(defun delete-line-or-region ()
  "Delete current line or region without putting it to kill-ring. When `universal-argument' is called first, duplicate whole buffer (but respect `narrow-to-region')."
  (interactive)
  (delete-region (line-or-region-point-min)
                 (line-or-region-point-max))
  (message "deleted"))

(defun line-or-region-point-min ()
  "Return min point of line or region. When `universal-argument' is called first, returns min point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-beginning)
        (line-beginning-position))
    (point-min)))

(defun line-or-region-point-max ()
  "Return max point of line or region. When `universal-argument' is called first, returns max point of whole buffer (but respect `narrow-to-region')."
  (if (null current-prefix-arg)
      (if (use-region-p)
          (region-end)
        (line-beginning-position 2))
    (point-max)))


;;; haskell functions

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
