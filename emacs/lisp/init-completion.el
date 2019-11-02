;;; init-completion.el --- auto completion feature -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 22 Oct 2019
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

(require 'lib-list)

(use-package company
  :defer 2
  :diminish
  :commands (company-complete-common company-manual-begin company-grab-line)
  :init
  (add-hook 'post-self-insert-hook #'+company-mode-maybe)
  (setq-default
   company-idle-delay nil
   company-tooltip-limit 14
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case nil
   company-dabbrev-code-other-buffers t
   company-tooltip-align-annotations t
   company-require-match 'never
   company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode)
   company-backends '(company-capf)
   company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend))
  :config
  (setq company-idle-delay 0.1)
  (add-hook 'company-mode-hook #'+company-init-backends)
  (global-company-mode +1))

(defun +company-mode-maybe ()
  "Enable company mode if it's not enabled."
  (unless global-company-mode
    (global-company-mode +1)))

;;;###autoload
(defvar +company-backend-alist
  '((text-mode :derived (company-dabbrev company-yasnippet company-ispell))
    (prog-mode :derived (:separate company-capf company-yasnippet))
    (conf-mode :derived company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends.

The backends for any mode is built from this.")

;;;###autodef
(defun +company-set-backend (modes &rest backends)
  "Prepend BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major
or minor modes.  This will overwrite backends for MODES on
consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (+company-set-backend 'js2-mode
    'company-tide 'company-yasnippet)

  (+company-set-backend 'sh-mode
    '(company-shell :with company-yasnippet))

  (+company-set-backend '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (+company-set-backend 'sh-mode nil)  ; unsets backends for `sh-mode'

To have BACKENDS apply to any mode that is a parent of MODES, set MODES to
:derived, e.g.

  (+company-set-backend! :derived 'text-mode 'company-dabbrev 'company-yasnippet)"
  (declare (indent defun))
  (let ((type :exact))
    (when (eq modes :derived)
      (setq type :derived
            modes (pop backends)))
    (dolist (mode (+enlist modes))
      (if (null (car backends))
          (setq +company-backend-alist
                (delq (assq mode +company-backend-alist)
                      +company-backend-alist))
        (setf (alist-get mode +company-backend-alist)
              (cons type backends))))))

(defun +company--backends ()
  "Return the list of `company' backends."
  (append (cl-loop for (mode . rest) in +company-backend-alist
                   for type = (car rest)
                   for backends = (cdr rest)
                   if (or (and (eq type :derived) (derived-mode-p mode)) ; parent modes
                          (and (eq type :exact)
                               (or (eq major-mode mode) ; major modes
                                   (and (boundp mode)
                                        (symbol-value mode))))) ; minor modes
                   append backends)
          (default-value 'company-backends)))

;;;###autoload
(defun +company-init-backends ()
  "Set `company-backends' for the current buffer."
  (unless (eq major-mode 'fundamental-mode)
    (set (make-local-variable 'company-backends) (+company--backends)))
  (add-hook 'after-change-major-mode-hook #'+company-init-backends nil 'local))

(put '+company-init-backends 'permanent-local-hook t)

(provide 'init-completion)
;;; init-completion.el ends here
