;;; packages.el --- d12frosted-lisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2015-2017 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst d12frosted-lisp-packages
  '(emacs-lisp
    lispy))

(defun d12frosted-lisp/post-init-emacs-lisp ()
  ;; (setq lisp-indent-function 'common-lisp-indent-function)

  ;; Make Elisp regexp look nicer
  ;; http://oremacs.com/2015/01/11/pretty-elisp-regex/
  (defun fontify-glyph (item glyph)
    `((,item
       (0 font-lock-keyword-face t)
       (0 (prog1
              (compose-region (match-beginning 0)
                              (match-end 0)
                              ,glyph) nil)))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          (fontify-glyph "\\\\\\\\|" "∨"))
  (font-lock-add-keywords 'emacs-lisp-mode
                          (fontify-glyph "\\\\\\\\(" "("))
  (font-lock-add-keywords 'emacs-lisp-mode
                          (fontify-glyph "\\\\\\\\)" ")")))

(defun d12frosted-lisp/init-lispy ()
  (use-package lispy
    :commands (lispy-mode)
    :init
    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
    :config
    ;; C-M-m should be used as major mode leader
    ;; But M-<return> is lisp-meta-return
    (unbind-key "M-RET" lispy-mode-map)

    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "m" #'lispy-mark-symbol)
    (bind-key "C-a" 'mwim-beginning-of-code-or-line lispy-mode-map)
    (bind-key "C-e" 'mwim-end-of-line-or-code lispy-mode-map)))

;;; packages.el ends here
