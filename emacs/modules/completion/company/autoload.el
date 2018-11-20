;;; completion/company/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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

;;;###autoload
(defvar +company-backend-alist
  '((text-mode :derived (company-dabbrev company-yasnippet company-ispell))
    (prog-mode :derived (:separate company-capf company-yasnippet))
    (conf-mode :derived company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for
any mode is built from this.")

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major
or minor modes.  This will overwrite backends for MODES on
consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode

To have BACKENDS apply to any mode that is a parent of MODES, set MODES to
:derived, e.g.

  (set-company-backend! :derived 'text-mode 'company-dabbrev 'company-yasnippet)"
  (declare (indent defun))
  (let ((type :exact))
    (when (eq modes :derived)
      (setq type :derived
            modes (pop backends)))
    (dolist (mode (doom-enlist modes))
      (if (null (car backends))
          (setq +company-backend-alist
                (delq (assq mode +company-backend-alist)
                      +company-backend-alist))
        (setf (alist-get mode +company-backend-alist)
              (cons type backends))))))

(defun +company--backends ()
  (append (cl-loop for (mode . rest) in +company-backend-alist
                   for type = (car rest)
                   for backends = (cdr rest)
                   if (or (and (eq type :derived) (derived-mode-p mode)) ; parent modes
                          (and (eq type :exact)
                               (or (eq major-mode mode)  ; major modes
                                   (and (boundp mode)
                                        (symbol-value mode))))) ; minor modes
                   append backends)
          (default-value 'company-backends)))

;;;###autoload
(defun +company|init-backends ()
  "Set `company-backends' for the current buffer."
  (unless (eq major-mode 'fundamental-mode)
    (set (make-local-variable 'company-backends) (+company--backends)))
  (add-hook 'after-change-major-mode-hook #'+company|init-backends nil 'local))

(put '+company|init-backends 'permanent-local-hook t)
