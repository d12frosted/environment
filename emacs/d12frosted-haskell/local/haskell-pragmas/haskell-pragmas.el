;;; haskell-pragmas.el --- Utilities for working with Haskell language pragmas.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'haskell-mode)
(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'evil-define-key "evil-core")

(defvar haskell-pragmas--language-pragmas nil)

(defun haskell-pragmas-language-pragmas ()
  (unless haskell-pragmas--language-pragmas
    ;; Retrieve list of language pragmas from stack.
    (let ((str (s-split "\n" (s-trim (shell-command-to-string "stack ghc -- --supported-languages")))))
      (setq haskell-pragmas--language-pragmas str)))

  haskell-pragmas--language-pragmas)

(defun haskell-pragmas-in-file ()
  (--filter (s-matches?
             (rx-to-string `(and "{-# LANGUAGE" (+ space) (* nonl) ,it))
             (buffer-string))
            (haskell-pragmas-language-pragmas)))

(defun haskell-pragmas--available-language-pragmas ()
  (-difference (haskell-pragmas-language-pragmas) (haskell-pragmas-in-file)))

(defun haskell-pragmas--goto-buffer-start ()
  (goto-char (point-min))

  ;; Skip #! line
  (when (and (s-matches? (rx bol "#!") (cb-buffers-current-line))
             (search-forward "#!" nil t))
    (goto-char (line-end-position))
    (forward-char 1))

  (while (and (not (eobp))
              (s-blank? (cb-buffers-current-line)))
    (forward-line 1)))

;;;###autoload
(defun haskell-pragmas-insert (pragma)
  "Read a language PRAGMA to be inserted at the start of this file."
  (interactive (list (completing-read "Pragma: "
                                      (haskell-pragmas--available-language-pragmas)
                                      nil t)))
  (let ((s (format "{-# LANGUAGE %s #-}\n" pragma)))
    (save-excursion
      (haskell-pragmas--goto-buffer-start)

      (insert s))))

;;;###autoload
(defun haskell-pragmas-init ()
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal haskell-mode-map
      (kbd "SPC i L") #'haskell-pragmas-insert))
  (when (fboundp 'spacemacs/set-leader-keys-for-major-mode)
    (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "il" #'haskell-pragmas-insert)))

(provide 'haskell-pragmas)

;;; haskell-pragmas.el ends here
