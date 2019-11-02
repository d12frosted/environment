;;; init-buffer.el --- buffer -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 21 Oct 2019
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
(defvar +buffer-fallback-name "*scratch*"
  "The name of the buffer to fall back to.

Used when no other buffers exist.")

;;;###autoload
(defun +buffer-fallback ()
  "Return the fallback buffer, creating it if necessary.

By default this is the scratch buffer. See
`+buffer-fallback-name' to change this."
  (get-buffer-create +buffer-fallback-name))

;;;###autoload
(defun +buffer-display-and-switch (buffer-or-name)
  "Display BUFFER-OR-NAME in some window and switch to it."
  (select-window (display-buffer buffer-or-name)))

(provide 'init-buffer)
;;; init-buffer.el ends here
