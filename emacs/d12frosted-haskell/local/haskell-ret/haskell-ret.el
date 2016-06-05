;;; haskell-ret.el --- Define a smart newline command for Haskell.  -*- lexical-binding: t; -*-

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
(require 'haskell)
(require 's)
(require 'thingatpt)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'cb-buffers-in-string-or-comment? "cb-buffers")
(autoload 'evil-define-key "evil-core")
(autoload 'haskell-indentation-newline-and-indent "haskell-indentation")
(autoload 'sp-get-enclosing-sexp "smartparens")

(defun haskell-ret--sp-beg (&optional sexp)
  (plist-get (or sexp (sp-get-enclosing-sexp)) :beg))

(defun haskell-ret--just-after-open-op? (op)
  (s-matches? (rx-to-string `(and ,op (* space) eos)) (buffer-substring (line-beginning-position) (point))))

(defun haskell-ret--sp-inside-sexp? (expected-op &optional same-line? sexp)
  (-let [(&plist :beg beg :end end :op actual-op) (or sexp (sp-get-enclosing-sexp))]
    (when (equal expected-op actual-op)
      (if same-line?
          (= (line-number-at-pos beg) (line-number-at-pos end))
        t))))

(defun haskell-ret--sp-inside-curly-braces? (&optional same-line? sexp)
  (haskell-ret--sp-inside-sexp? "{" same-line? sexp))

(defun haskell-ret--sp-inside-square-braces? (&optional same-line? sexp)
  (haskell-ret--sp-inside-sexp? "[" same-line? sexp))

(defun haskell-ret--split-braced-expression-over-new-lines (sexp)
  (-let [(&plist :beg beg :end end :op op) sexp]
    (save-excursion
      (goto-char (1- end))
      (newline-and-indent)
      (goto-char (1+ beg))
      (just-one-space)
      (let ((beg (haskell-ret--sp-beg)))
        (while (and (search-forward-regexp (rx ",") nil t)
                    (<= beg (haskell-ret--sp-beg)))
          (when (equal beg (haskell-ret--sp-beg))
            (unless (cb-buffers-in-string-or-comment?)
              (forward-char -1)
              (insert "\n")
              (indent-according-to-mode)
              (search-forward ",")
              (just-one-space))))))

    ;; If point was after the opening brace before splitting, it will not have
    ;; moved to the next line. Correct this by moving forward to indentation on
    ;; the next line.
    (when (haskell-ret--just-after-open-op? op)
      (forward-line)
      (back-to-indentation))))

;;;###autoload
(defun haskell-ret (&optional arg)
  "Insert a newline with context-sensitive formatting.

With prefix arg ARG, just insert a newline and indent."
  (interactive "P")
  (let ((sexp (sp-get-enclosing-sexp)))
    (cond
     (arg
      (newline-and-indent))

     ((s-matches? (rx bol (? ">") (* space) "--") (cb-buffers-current-line))
      (insert "\n-- "))

     ((or (haskell-ret--sp-inside-curly-braces? t sexp)
          (haskell-ret--sp-inside-square-braces? t sexp))
      (haskell-ret--split-braced-expression-over-new-lines sexp))

     ((and (or (haskell-ret--sp-inside-curly-braces? nil sexp)
               (haskell-ret--sp-inside-square-braces? nil sexp))
           (thing-at-point-looking-at (rx (or "[" "{") (* space))))
      (goto-char (1+ (haskell-ret--sp-beg sexp)))
      (newline-and-indent)
      (insert "  "))

     (t
      (call-interactively #'haskell-indentation-newline-and-indent)))))

;;;###autoload
(defun haskell-ret-init ()
  (evil-define-key 'insert haskell-mode-map (kbd "<return>") #'haskell-ret))

(provide 'haskell-ret)

;;; haskell-ret.el ends here
