;;; packages.el --- d12frosted-lisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
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
      ;; unbind some key bindings
      (unbind-key "M-RET" lispy-mode-map)
      (bind-key "C-a" 'mwim-beginning-of-code-or-line lispy-mode-map)))

;;; packages.el ends here
