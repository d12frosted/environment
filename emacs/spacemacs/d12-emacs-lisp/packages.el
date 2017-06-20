;;; packages.el --- d12-emacs-lisp layer packages file for Spacemacs.
;;
;;; Copyright (c) 2015-2017 Boris Buliga
;;
;;; Author: Boris Buliga <boris@d12frosted.io>
;;
;;; URL: https://github.com/d12frosted/environment/emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst d12-emacs-lisp-packages
  '(emacs-lisp
    lispy)
  "The list of Lisp packages required by the d12-emacs-lisp layer.")

(defun d12-emacs-lisp/post-init-emacs-lisp ()
  ;; Make Elisp regexp look nicer
  ;; http://oremacs.com/2015/01/11/pretty-elisp-regex/
  (defun fontify-glyph (item glyph)
    `((,item
       (0 font-lock-keyword-face t)
       (0 (prog1
              (compose-region (match-beginning 0)
                              (match-end 0)
                              ,glyph)
            nil)))))

  (font-lock-add-keywords 'emacs-lisp-mode
                          (fontify-glyph "\\\\\\\\|" "∨"))
  (font-lock-add-keywords 'emacs-lisp-mode
                          (fontify-glyph "\\\\\\\\(" "("))
  (font-lock-add-keywords 'emacs-lisp-mode
                          (fontify-glyph "\\\\\\\\)" ")")))

(defun d12-emacs-lisp/init-lispy ()
  (use-package lispy
    :commands (lispy-mode)
    :init
    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
    :config
    (d12-key-unbind "M-RET" lispy-mode-map-lispy)
    (d12-key-copy "C-a" lispy-mode-map)
    (d12-key-copy "C-e" lispy-mode-map)))

;;; packages.el ends here
