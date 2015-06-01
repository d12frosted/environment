;;; python-funcs.el --- funcs file of python configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 27 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(defun python-default ()
  (setq mode-name "Python"
        tab-width 2
        ;; auto-indent on colon doesn't work well with if statement
        electric-indent-chars (delq ?: electric-indent-chars))
  (annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent))

(defun annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import i?pdb")
  (highlight-lines-matching-regexp "i?pdb.set_trace()"))

(defun python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (if (executable-find "ipdb")
                   "import ipdb; ipdb.set_trace()"
                 "import pdb; pdb.set_trace()"))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert-string trace)
        (insert-string "\n")
        (python-indent-line)))))

(defun python-setup-shell ()
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            ;; python-shell-interpreter-args (if (system-is-mac)
            ;;                                   "--gui=osx --matplotlib=osx --colors=Linux"
            ;;                                 (if (system-is-linux)
            ;;                                     "--gui=wx --matplotlib=wx --colors=Linux"))
            python-shell-prompt-regexp "In \\[[0-9]+\\]: "
            python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
            python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
            python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
            python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    (setq python-shell-interpreter "python")))

(defun python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(defun python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-defun nil)
  (python-shell-switch-to-shell))

(defun python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell))

(defun python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (python-shell-switch-to-shell))

(defun spacemacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (setq universal-argument t)
  (if arg
      (call-interactively 'compile)

    (setq compile-command (format "python %s" (file-name-nondirectory
                                               buffer-file-name)))
    (compile compile-command t)
    (with-current-buffer (get-buffer "*compilation*")
      (inferior-python-mode))))

(defun spacemacs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
  (interactive "P")
  (spacemacs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer))
