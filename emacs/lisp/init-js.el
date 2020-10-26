;;; init-js.el --- JavaScript support(?) -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Jun 2020
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

(require 'init-package)
(require 'init-completion)
(require 'init-lsp)

(use-package js
  :defer t
  :straight (:type built-in)
  :init
  (setq js-indent-level 2))

(use-package tide
  :defer t
  :commands (tide-setup)
  :config
  (with-eval-after-load 'company
    ;; tide affects the global `company-backends', undo this
    (setq-default company-backends (delq 'company-tide (default-value 'company-backends))))
  (+company-set-backend 'tide-mode 'company-tide))

(defun +js-init-lsp-or-tide-maybe-h ()
  "Start `lsp' or `tide' in the current buffer.

LSP will be used if the flag is enabled for JavaScript AND if the
current buffer represents a file in a project.

If LSP fails to start (e.g. no available server or project), then
we fall back to tide."
  (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
    (when (or (derived-mode-p 'js-mode 'typescript-mode)
              (and buffer-file-name
                   (eq major-mode 'web-mode)
                   (string= "tsx" (file-name-extension buffer-file-name))))
      (if (not buffer-file-name)
          ;; necessary because `tide-setup' and `lsp' will error if not a
          ;; file-visiting buffer
          (add-hook 'after-save-hook #'+js-init-lsp-or-tide-maybe-h nil 'local)
        (or (lsp)
            ;; fall back to tide
            (if (executable-find "node")
                (and (require 'tide nil t)
                     (progn (tide-setup) tide-mode))
              (ignore
               (message "Couldn't start tide because 'node' is missing"))))
        (remove-hook 'after-save-hook #'+js-init-lsp-or-tide-maybe-h 'local)))))

(use-package js2-mode
  :defer t
  :hook ((js2-mode . +js-init-lsp-or-tide-maybe-h)))

(use-package typescript-mode
  :defer t
  :hook ((typescript-mode . +js-init-lsp-or-tide-maybe-h))
  :init
  (setq typescript-indent-level 2))


(provide 'init-js)
;;; init-js.el ends here
