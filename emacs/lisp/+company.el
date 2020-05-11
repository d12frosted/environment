;;; +company.el --- functions to master company -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 11 May 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'lib-list)

;;;###autoload
(defvar +company-backend-alist
  '((text-mode company-dabbrev company-yasnippet company-ispell)
    (prog-mode company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends.

The backends for any mode is built from this.")

(defvar company-backends)

;;;###autoload
(defun +company-init-backends ()
  "Set `company-backends' for the current buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
      buffer-read-only
      (setq-local company-backends (+company--backends))))

(defun +company--backends ()
  "Get `company-backends' for the current buffer."
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

(defun set-company-backend! (modes &rest backends)
  "Prepend BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major
or minor modes. This will overwrite backends for MODES on
consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (set-company-backend! 'sh-mode nil)  ; unsets backends for `sh-mode'"
  (declare (indent defun))
  (dolist (mode (+enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))

(provide '+company)
;;; +company.el ends here
