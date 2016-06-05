;;; haskell-meta-ret.el --- Define a smart meta-ret command for Haskell. -*- lexical-binding: t; -*-

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
(require 'thingatpt)

(autoload 'cb-buffers-current-line "cb-buffers")
(autoload 'cb-buffers-in-string-or-comment? "cb-buffers")
(autoload 'evil-define-key "evil-core")
(autoload 'evil-forward-word-begin "evil-commands")
(autoload 'evil-insert-state "evil-states")
(autoload 'haskell-indentation-indent-line "haskell-indentation")
(autoload 'haskell-indentation-newline-and-indent "haskell-indentation")
(autoload 'haskell-parser-parse-typesig "haskell-parser")
(autoload 'haskell-unicode-use-unicode-symbols? "haskell-unicode")
(autoload 'shm/forward-node "shm")
(autoload 'shm/goto-parent-end "shm")
(autoload 'shm/reparse "shm")
(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'yas-exit-all-snippets "yasnippet")
(autoload 'yas-expand-snippet "yasnippet")

(defconst haskell-meta-ret--haskell-keywords
  '("let" "where" "module" "case" "class" "data" "deriving" "default"
    "import" "infixl" "infixr" "newtype" "data" "type" "if" "then" "else"))

(defun haskell-meta-ret--line-content-relative (move-n-lines)
  "Return the line at point, or another line relative to this line.
MOVE-N-LINES is an integer that will return a line forward if
positive or backward if negative."
  (save-excursion
    (forward-line move-n-lines)
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun haskell-meta-ret--at-record-decl-data-header? ()
  (when (s-matches? (rx bol (? ">") (* space) "data" space) (cb-buffers-current-line))
    (shm/reparse)
    (save-excursion
      (back-to-indentation)
      (shm/goto-parent-end)
      (s-matches? "}" (cb-buffers-current-line)))))

(defun haskell-meta-ret--newline-indent-to-same-col ()
  "Make a new line below the current one and indent to the same column."
  (let ((col (save-excursion (back-to-indentation) (current-column))))
    (goto-char (line-end-position))
    (newline)
    (when (s-ends-with? ".lhs" (buffer-name))
      (insert "> "))
    (indent-to col)))

(defun haskell-meta-ret--first-ident-on-line ()
  (car (-difference (s-split (rx (? ">") space) (cb-buffers-current-line) t)
                    haskell-meta-ret--haskell-keywords)))

(defun haskell-meta-ret--snippet-text-for-typesig (fname parsed-typesig)
  (-if-let* (((&plist :args args) parsed-typesig)
             (as (--map-indexed (format "${%s:x%s}" (1+ it-index) it-index) args)))
      (format "%s %s = ${%s:undefined}" fname (s-join " " as) (1+ (length as)))
    (format "%s $0" fname)))

(defun haskell-meta-ret--insert-function-template (fname parsed-typesig)
  (back-to-indentation)
  (when (thing-at-point-looking-at "where")
    (evil-forward-word-begin))
  (let ((col 0))
    (setq col (current-column))
    (shm/reparse)
    (shm/goto-parent-end)

    (goto-char (line-end-position))
    (newline)
    (when (s-ends-with? ".lhs" (buffer-file-name))
      (insert "> "))
    (let ((snippet (haskell-meta-ret--snippet-text-for-typesig fname parsed-typesig)))
      (indent-to col)
      (yas-expand-snippet snippet nil nil '((yas/indent-line 'fixed))))))

(defun haskell-meta-ret--at-decl-for-function? (fname)
  (when fname
    (or
     ;; A type decl exists in this buffer?
     (s-matches? (eval `(rx bol (? ">") (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ space) (or "∷" "::")))
                 (buffer-string))
     ;; At an equation?
     (s-matches? (eval `(rx bol (? ">") (* space)
                            (? (or "let" "where") (+ space))
                            ,fname (+ nonl) "="))
                 (cb-buffers-current-line)))))

(defun haskell-meta-ret--back-to-function-typesig (fname)
  (let ((function-decl-rx
         (rx-to-string `(and bol (? ">") (* space) (? (or "let" "where") (+ space))
                             (group-n 1 ,fname) (+ space) (or "∷" "::")))))
    (cond
     ((s-matches? function-decl-rx (cb-buffers-current-line))
      (goto-char (line-beginning-position))
      (search-forward-regexp function-decl-rx)
      (goto-char (match-beginning 1)))
     ((search-backward-regexp function-decl-rx nil t)
      (goto-char (match-beginning 1))))))

(defun haskell-meta-ret--in-data-decl? ()
  (cond
   ((cb-buffers-in-string-or-comment?) nil)
   ((s-matches? "}" (buffer-substring (line-beginning-position) (point))) nil)
   ((thing-at-point-looking-at (rx bol (? ">") (* space) "data ")) t)
   (t
    (save-excursion
      (when (search-backward-regexp (rx bol (? ">") (not space)) nil t)
        (thing-at-point-looking-at "data "))))))

(defun haskell-meta-ret--at-end-of-record-decl? ()
  (save-excursion
    (goto-char (line-beginning-position))
    (and (haskell-meta-ret--in-data-decl?)
         (s-matches? (rx "}" (* space) eol) (cb-buffers-current-line)))))

(defun haskell-meta-ret--insert-deriving-clause ()
  (goto-char (line-end-position))
  (when (s-matches? (rx (not space) (* space) "}" (* space) eol)
                    (cb-buffers-current-line))
    (search-backward "}")
    (newline-and-indent)
    (goto-char (line-end-position)))

  (just-one-space)
  (insert "deriving ()")
  (forward-char -1))

(defun haskell-meta-ret--insert-record-field ()
  (let ((underscore-prefix-style?
         (s-matches? (rx bol (? ">") (* space) (? (or "{" ",")) (* space) "_") (cb-buffers-current-line)))

        (inserting-first-field? (sp-inside-curly-braces-blank-content?))

        (brace-or-comma-column
         (save-excursion
           (goto-char (line-beginning-position))
           (cond ((and (search-forward-regexp (rx (or "," "{")) nil t)
                       (sp-inside-curly-braces?))
                  (forward-char -1)
                  (current-column))
                 (t 2)))))

    (goto-char (line-end-position))
    (if inserting-first-field?
        (just-one-space)
      (newline)
      (indent-to-column brace-or-comma-column))

    (yas-expand-snippet
     (format "%s%s${1:field} %s ${2:T}"
             (if inserting-first-field? "" ", ")
             (if underscore-prefix-style? "_" "")
             (haskell-meta-ret--fmt-::)))))

(defun haskell-meta-ret--fmt-::     () (if (haskell-unicode-use-unicode-symbols?) "∷" "::"))
(defun haskell-meta-ret--fmt-rarrow () (if (haskell-unicode-use-unicode-symbols?) "→" "->"))
(defun haskell-meta-ret--fmt-larrow () (if (haskell-unicode-use-unicode-symbols?) "←" "<-"))

(defun haskell-meta-ret--parse-function-decl (fname)
  (save-excursion
    (when (haskell-meta-ret--back-to-function-typesig fname)
      (shm/reparse)
      (let* ((start (point))
             (end (progn (shm/goto-parent-end) (point)))
             (end (if (= end (point)) (line-end-position) end))
             (typesig (buffer-substring-no-properties start end)))

        (haskell-parser-parse-typesig typesig)))))

;;;###autoload
(defun haskell-meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond
   ;; Append new record field
   ((and (haskell-meta-ret--at-record-decl-data-header?)
         (or (s-matches? (rx "{" (* space) eol) (cb-buffers-current-line))
             (s-matches? (rx bol (* space) "{") (haskell-meta-ret--line-content-relative +1))))
    (search-forward "{")
    (-let [(&plist :end end) (sp-get-enclosing-sexp)]
      (goto-char (1- end))
      (when (sp-inside-curly-braces? t)
        (newline))
      (forward-line -1)
      (haskell-meta-ret--insert-record-field)
      (message "New field")))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (? ">") (* space) "data" (+ space)) (cb-buffers-current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "| ")
    (goto-char (line-beginning-position))
    (haskell-indentation-indent-line)
    (goto-char (line-end-position))
    (message "New data case"))

   ;; Insert new type decl case below the current one.
   ((s-matches? (rx bol (? ">") (* space) "|") (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert "| ")
    (message "New data case"))

   ;; Insert new alternative case
   ((s-matches? (rx bol (? ">") (* space) "<|>") (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert "<|> ")
    (message "New alternative"))

   ;; Insert new applicative case
   ((s-matches? (rx bol (? ">") (* space) "<$>") (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert "<*> ")
    (message "New applicative"))

   ;; Insert new applicative case
   ((s-matches? (rx bol (? ">") (* space) "<*>") (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert "<*> ")
    (message "New applicative"))

   ;; Insert new import
   ((s-matches? (rx bol (? ">") (* space) "import") (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert "import ")
    (message "New import"))

   ;; Insert new record field
   ((and (haskell-meta-ret--in-data-decl?)
         (or (s-matches? (rx bol (? ">") (* space) (or "{") (* space)) (cb-buffers-current-line))
             (s-matches? (rx "{" (* space) eol) (haskell-meta-ret--line-content-relative -1))))
    (haskell-meta-ret--insert-record-field)
    (message "New field"))

   ;; New function case.
   ((haskell-meta-ret--at-decl-for-function? (haskell-meta-ret--first-ident-on-line))
    (back-to-indentation)
    (let* ((fname (haskell-meta-ret--first-ident-on-line))
           (parsed (haskell-meta-ret--parse-function-decl fname)))
      (haskell-meta-ret--insert-function-template fname parsed)
      (message "New binding case")))

   ;; Insert deriving clause
   ((haskell-meta-ret--at-end-of-record-decl?)
    (haskell-meta-ret--insert-deriving-clause)
    (message "Deriving clause"))

   ;; Insert new record field
   ((and (haskell-meta-ret--in-data-decl?)
         (s-matches? (rx bol (? ">") (* space) (or "{" ",") (* space)) (cb-buffers-current-line)))
    (haskell-meta-ret--insert-record-field)
    (message "New field"))

   ;; Insert new line starting with comma.
   ((s-matches? (rx bol (? ">") (* space) ",") (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line starting with an arrow.
   ((s-matches? (rx bol (? ">") (* space) (or "→" "->")) (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert (format "%s " (haskell-meta-ret--fmt-rarrow)))
    (message "New arrow"))

   ;; Insert new pattern match case below the current one.
   ((s-matches? (rx bol (? ">") (* space) (+ (not (any "="))) (or "->" "→")) (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (yas-expand-snippet (format "${1:pat} %s $0" (haskell-meta-ret--fmt-rarrow)))
    (message "New pattern match case"))
   ((s-matches? (rx bol (? ">") (* space) "case" (+ space)) (cb-buffers-current-line))
    (newline-and-indent)
    (yas-expand-snippet (format "${1:pat} %s $0" (haskell-meta-ret--fmt-rarrow)))
    (message "New pattern match case"))

   ;; Insert new line starting with a comma for the current braced expr
   ((s-matches? (rx bol (? ">") (* space) (or "[" "{")) (cb-buffers-current-line))
    (haskell-meta-ret--newline-indent-to-same-col)
    (insert ", ")
    (message "New entry"))

   ;; Insert new line with a do-binding.
   ((s-matches? (rx bol (? ">") (* space) (+ nonl) (or "<-" "←")) (cb-buffers-current-line))
    (back-to-indentation)
    (let ((col (current-column)))
      (search-forward-regexp (rx  (or "<-" "←")))
      (shm/forward-node)
      (newline)
      (indent-to col))
    (yas-expand-snippet (format "${1:name} %s $0" (haskell-meta-ret--fmt-larrow)))
    (message "New do-binding"))

   (t
    (goto-char (line-end-position))
    (haskell-indentation-newline-and-indent)
    (message "New line")))

  (evil-insert-state))

;;;###autoload
(defun haskell-meta-ret-init ()
  (define-key haskell-mode-map (kbd "M-RET") #'haskell-meta-ret)
  (with-eval-after-load 'evil
    (evil-define-key 'normal haskell-mode-map (kbd "M-RET") #'haskell-meta-ret)
    (evil-define-key 'insert haskell-mode-map (kbd "M-RET") #'haskell-meta-ret)))

(provide 'haskell-meta-ret)

;;; haskell-meta-ret.el ends here
