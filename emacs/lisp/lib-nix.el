;;; lib-nix.el --- Utilities for using Nix -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 June 2021
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Various utilities for working with Nix.
;;
;;; Code:

(cl-defun nix-shell-command (&key
                             deps
                             command
                             message-intro
                             message-error)
  "Execute COMMAND in nix-shell with DEPS.

If result is non-zero, error is printed in messages buffer.

Basically, executes nix-shell -p DEPS --command COMMAND."
  (message message-intro)
  (let* ((error-buffer (generate-new-buffer "*nix-shell-error*"))
         (cmd (format
               "nix-shell -p %s --command '%s'"
               (string-join deps " ")
               command))
         (res (shell-command cmd nil error-buffer)))
    (unless (zerop res)
      (message (with-current-buffer error-buffer
                 (buffer-string)))
      (error message-error))))

(provide 'lib-nix)
;;; lib-nix.el ends here
