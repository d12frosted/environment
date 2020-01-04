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
(require 'init-package)

;;;###autoload
(defvar +company-backend-alist
  '((text-mode :derived (company-yasnippet))
    (prog-mode :derived (:separate company-capf company-yasnippet))
    (conf-mode :derived company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends.

The backends for any mode is built from this.")

(use-package company
  :defer 2
  :diminish
  :defines (company-backends)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line
             global-company-mode)
  :init
  (add-hook 'post-self-insert-hook #'+company-mode-maybe)
  (setq-default
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never
   company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode)
   company-backends '(company-capf)
   company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend))
  :config
  (add-hook 'company-mode-hook #'+company-init-backends)
  (global-company-mode +1))

(defun +company-mode-maybe ()
  "Enable company mode if it's not enabled."
  (unless global-company-mode
    (global-company-mode +1)))

(defun +company-set-backend (modes &rest backends)
  "Prepend BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major
or minor modes. This will overwrite backends for MODES on
consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (+company-set-backend 'js2-mode
    'company-tide 'company-yasnippet)

  (+company-set-backend 'sh-mode
    '(company-shell :with company-yasnippet))

  (+company-set-backend '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  ; unsets backends for `sh-mode'
  (+company-set-backend 'sh-mode nil)"
  (declare (indent defun))
  (dolist (mode (+enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))

(defun +company--backends ()
  "Return the list of `company' backends."
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode) ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

;;;###autoload
(defun +company-init-backends ()
  "Set `company-backends' for the current buffer."
  (if (not company-mode)
      (remove-hook 'change-major-mode-after-body-hook #'+company-init-backends 'local)
    (unless (eq major-mode 'fundamental-mode)
      (setq-local company-backends (+company--backends)))
    (add-hook 'change-major-mode-after-body-hook #'+company-init-backends nil 'local)))

(put '+company-init-backends 'permanent-local-hook t)

(provide 'init-completion)
;;; init-completion.el ends here
