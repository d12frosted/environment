;;; lib-buffer.el --- Utilities for working with buffers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 17 Mar 2021
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
;; These functions are unlikely to be used directly somewhere in my
;; configurations, but I use them when I need to quickly hack some
;; solution.
;;
;;; Code:

;;;###autoload
(defun buffer-content (buffer-or-name)
  "Return content of BUFFER-OR-NAME.

Resulting string is full of properties. Consider using
`substring-no-properties':

  (substring-no-properties (buffer-content BUFFER-OR-NAME))"
  (with-current-buffer buffer-or-name
    (buffer-substring (point-min)
                      (point-max))))

;;;###autoload
(defun buffer-lines (buffer-or-name)
  "Return lines of BUFFER-OR-NAME.

Each line is a string with properties. Trailing newline character
is not present."
  (buffer-lines-map buffer-or-name #'identity))

;;;###autoload
(defun buffer-lines-map (buffer-or-name fn)
  "Call FN on each line of BUFFER-OR-NAME and return resulting list.

As opposed to `buffer-lines-each', this function accumulates
result.

Each line is a string with properties. Trailing newline character
is not present."
  (declare (indent 1))
  (with-current-buffer buffer-or-name
    (goto-char (point-min))
    (let ((result))
      (while (not (eobp))
        (setq
         result
         (cons
          (funcall fn (buffer-substring
                       (line-beginning-position)
                       (line-end-position)))
          result))
        (forward-line))
      (reverse result))))

;;;###autoload
(defun buffer-lines-each (buffer-or-name fn)
  "Call FN on each line of BUFFER-OR-NAME.

As opposed to `buffer-lines-map', this function does not
accumulate any result and should used for side-effects.

Each line is a string with properties. Trailing newline character
is not present."
  (declare (indent 1))
  (with-current-buffer buffer-or-name
    (goto-char (point-min))
    (while (not (eobp))
      (funcall fn (buffer-substring
                   (line-beginning-position)
                   (line-end-position)))
      (forward-line))))

;;;###autoload
(defun buffer-lines-transform (buffer-or-name fn)
  "Call FN on each line of BUFFER-OR-NAME.

Line is replaced with result of calling FN.

Each line is a string with properties. Trailing newline character
is not present."
  (declare (indent 1))
  (with-current-buffer buffer-or-name
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((pos0 (line-beginning-position))
             (pos1 (line-end-position))
             (line (buffer-substring pos0 pos1))
             (result (funcall fn line)))
        (kill-region pos0 pos1)
        (insert result))
      (forward-line))))

;;;###autoload
(defun buffer-lines-each-t (buffer-or-name fn)
  "Call FN on each line of BUFFER-OR-NAME and replace it by result.

Each line is a string with properties. Trailing newline character
is not present."
  (declare (indent 1))
  (with-current-buffer buffer-or-name
    (goto-char (point-min))
    (let (l0 l1 s)
      (while (not (eobp))
        (setq l0 (line-beginning-position))
        (setq l1 (line-end-position))
        (setq s (buffer-substring l0 l1))
        (replace-region-contents l0 l1 (lambda () (funcall fn s)))
        (forward-line)))))

;;;###autoload
(defun buffer-generate (name &optional unique inhibit-buffer-hooks)
  "Create and return a buffer with a name based on NAME.

Unless UNIQUE, choose the buffer’s name using
‘generate-new-buffer-name’. Otherwise if buffer with NAME already
exists, recreate it.

See ‘get-buffer-create’ for the meaning of INHIBIT-BUFFER-HOOKS."
  (when unique
    (when-let ((buffer (get-buffer name)))
      (kill-buffer buffer)))
  (generate-new-buffer name inhibit-buffer-hooks))

;;;###autoload
(defun buffer-display-result-with (name &rest lines)
  "Generate and switch to a buffer with NAME and fill it with LINES."
  (declare (indent 1))
  (let ((buffer (apply #'buffer-generate-result-with name lines)))
    (switch-to-buffer buffer)
    (goto-char (point-min))))

;;;###autoload
(defun buffer-generate-result-with (name &rest lines)
  "Generate a unique buffer with NAME and fill it with LINES.

Return generated buffer."
  (declare (indent 1))
  (let ((buffer (buffer-generate name 'unique)))
    (with-current-buffer buffer
      (insert
       (string-join lines "\n"))
      (result-present-mode))
    buffer))

;;;###autoload
(define-minor-mode result-present-mode
  "Local minor mode for presenting results.

Basically, a `read-only-mode' with some goodies."
  :global nil
  :init-value nil
  (let ((enabled result-present-mode))
    (cond
     (enabled
      (read-only-mode +1))
     (t
      (read-only-mode -1)))))

(provide 'lib-buffer)
;;; lib-buffer.el ends here
