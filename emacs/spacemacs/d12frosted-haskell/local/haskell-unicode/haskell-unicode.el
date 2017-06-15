;;; haskell-unicode.el --- Unicode utils for Haskell editing.  -*- lexical-binding: t; -*-

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
(require 's)

(autoload 'cb-buffers-in-string-or-comment? "cb-buffers")
(autoload 'haskell-pragmas-in-file "haskell-pragmas")

(defun haskell-unicode-use-unicode-symbols? ()
  (-contains? (haskell-pragmas-in-file) "UnicodeSyntax"))

(defun haskell-unicode-rewrite-symbol (sym repl)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp (rx-to-string `(and (or "(" space)
                                                        (group ,sym)
                                                        (or space ")" eol))
                                                  t)
                                    nil t)

        (unless (cb-buffers-in-string-or-comment?)
          (replace-match repl t t nil 1))))))

;;;###autoload
(defun haskell-unicode-apply-to-buffer (&optional force)
  "Apply transformations to the entire buffer if unicode syntax is enabled.

With optional argument FORCE, apply unconditionally."
  (interactive "P")
  (when (or force (haskell-unicode-use-unicode-symbols?))
    (--each '(("->" "→")
              ("=>" "⇒")
              ("<-" "←")
              ("::" "∷")
              ("forall" "∀"))
      (apply 'haskell-unicode-rewrite-symbol it))))

;;; Font locking

(defun haskell-unicode--font-lock-replace-match (regex group replacement)
  (list regex
        `(0 (progn (compose-region (match-beginning ,group) (match-end ,group)
                                   ,replacement 'decompose-region)
                   nil))))

(defun haskell-unicode--apply-font-lock ()
  (font-lock-add-keywords
   nil
   `((,(rx (or bol space "}" ")" "]")
           (group "$" (? "!"))
           (or eol space "{" "(" "["))
      1 'font-lock-comment-face)

     (,(rx (not (any "(")) (group ",") (not (any ")")))
      1 'font-lock-comment-face)

     (";" . font-lock-comment-face)

     ("∀" . font-lock-keyword-face)

     (,(rx bol (* space) "type" (+ space) "family" eow) . font-lock-keyword-face)

     ,(haskell-unicode--font-lock-replace-match (rx (or (and space (group-n 1 ".") space)
                                         (and "(" (group-n 1 ".") ")")
                                         ))
                                 1 "·")

     ,(haskell-unicode--font-lock-replace-match (rx space (group "<-") (or space eol)) 1 "←")
     ,(haskell-unicode--font-lock-replace-match (rx (or space "(") (group "->") (or ")" space eol)) 1 "→")
     ,(haskell-unicode--font-lock-replace-match (rx space (group "=>") (or space eol)) 1 "⇒")
     ,(haskell-unicode--font-lock-replace-match (rx space (group "::") (or space eol)) 1 "∷")

     ;; Lambda forms
     ,(haskell-unicode--font-lock-replace-match
       (rx (group "\\") (and (* space)
                             (or word "_" (and "(" (* nonl) ")"))
                             (*? nonl))
           (* space) (or "->" "→"))
       1 (propertize "λ" 'face 'font-lock-keyword-face)))))

;;;###autoload
(defun haskell-unicode-init ()
  (haskell-unicode--apply-font-lock)
  (add-hook 'ghc-type-dump-mode-hook #'haskell-unicode--apply-font-lock)
  (add-hook 'haskell-interactive-mode-hook #'haskell-unicode--apply-font-lock)
  (add-hook 'before-save-hook #'haskell-unicode-apply-to-buffer nil t)
  (add-hook 'evil-insert-state-exit-hook #'haskell-unicode-apply-to-buffer nil t))

(provide 'haskell-unicode)

;;; haskell-unicode.el ends here
