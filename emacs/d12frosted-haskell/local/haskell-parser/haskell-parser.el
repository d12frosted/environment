;;; haskell-parser.el --- Syntax parser utilities for Haskell.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.9.0") (dash "2.10.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Syntax parser utilities for Haskell.

;;; Code:

(require 's)
(require 'dash)
(require 'thingatpt)

(defun haskell-parser-parse-typesig (typesig)
  "Parse the given type signature TYPESIG into plist with the following keys:

- :forall - a list of quantified type variables
- :constraints - a list of typeclass constraints
- :args - a list of argument types
- :return-type - the return type of this function.

If there are no arguments, the type of the equation is the return-type."
  (with-temp-buffer
    (insert typesig)
    (goto-char (point-min))
    (let ((_           (haskell-parser--consume-whitespace))
          (_           (haskell-parser--consume-::))
          (forall      (haskell-parser--parse-forall))
          (_           (haskell-parser--consume-whitespace))
          (constraints (haskell-parser--parse-constraints))
          (_           (haskell-parser--consume-whitespace))
          (types       (haskell-parser--parse-types)))

      (list :forall forall
            :constraints constraints
            :args (-butlast types)
            :return-type (-last-item types)))))

(defun haskell-parser--consume-:: ()
  (search-forward-regexp (rx (or "∷" "::")))
  (haskell-parser--consume-whitespace))

(defun haskell-parser--parse-forall ()
  (when (thing-at-point-looking-at (rx (or "forall" "∀")))
    (let* ((start (search-forward-regexp (rx (or "forall" "∀") (* space))))
           (end (1- (search-forward ".")))
           (vars (buffer-substring-no-properties start end)))
      (->> (s-split (rx space) vars t)
           (-map 'haskell-parser--chop-birdtrack)))))

(defun haskell-parser--parse-constraints ()
  (when (s-matches? (rx (or "⇒" "=>"))
                    (buffer-substring (point) (point-max)))
    (prog1 (->> (haskell-parser--parse-constraint-exprs)
                (s-chop-prefix "(")
                (s-chop-suffix ")")
                (s-split (rx (* space) "," (* space)))
                (-map 'haskell-parser--chop-birdtrack))
      (haskell-parser--consume-constraint-arrow))))

(defun haskell-parser--parse-constraint-exprs ()
  (let ((start (point)))
    (haskell-parser--consume-whitespace)
    (while (not (or (eobp) (eolp)
                    (thing-at-point-looking-at (rx (or "," "--" "⇒" "=>")))))
      (when (thing-at-point 'sexp)
        (forward-sexp)
        (haskell-parser--consume-whitespace)))
    (while (s-matches? (rx space) (char-to-string (char-before)))
      (forward-char -1))

    (buffer-substring start (point))))

(defun haskell-parser--chop-birdtrack (s)
  (s-chop-suffix "\n>" s))

(defun haskell-parser--consume-whitespace ()
  (let (consumed?)
    (when (haskell-parser--consume-birdtrack)
      (setq consumed? t))

    (while (and (not (eobp))
                (or (eolp)
                    (s-matches? (rx space) (char-to-string (char-after)))))
      (forward-char)
      (haskell-parser--consume-birdtrack)
      (setq consumed? t))

    consumed?))

(defun haskell-parser--consume-birdtrack ()
  (when (and (bolp) (not (eobp))
             (equal (char-after) ?>))
    (forward-char)
    t))

(defun haskell-parser--consume-comments ()
  (while (or (haskell-parser--consume-brace-comment)
             (haskell-parser--consume-line-comment))
    (haskell-parser--consume-newline-and-birdtrack)))

(defun haskell-parser--consume-brace-comment ()
  (let ((start (point)))
    (while (and (not (eobp))
                (thing-at-point-looking-at (rx (* space) "{-")))
      (haskell-parser--consume-whitespace)
      (forward-sexp))
    (let ((end (point)))
      (when (/= start end)
        (buffer-substring start end)))))

(defun haskell-parser--consume-line-comment ()
  (let ((start (point)))
    (while (and (not (eobp))
                (thing-at-point-looking-at (rx (* space) "--")))
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-char)))
    (let ((end (point)))
      (when (/= start end)
        (buffer-substring start end)))))

(defun haskell-parser--consume-newline-and-birdtrack ()
  (when (and (eolp) (not (eobp)))
    (forward-char)
    (haskell-parser--consume-birdtrack)
    t))

(defun haskell-parser--consume-arrow ()
  (haskell-parser--consume-whitespace)
  (cond
   ((thing-at-point-looking-at "→")  (forward-char 1))
   ((thing-at-point-looking-at "->") (forward-char 2)))
  (haskell-parser--consume-whitespace))

(defun haskell-parser--consume-constraint-arrow ()
  (haskell-parser--consume-whitespace)
  (cond
   ((thing-at-point-looking-at "⇒")  (forward-char 1))
   ((thing-at-point-looking-at "=>") (forward-char 2)))
  (haskell-parser--consume-whitespace))

(defun haskell-parser--parse-types ()
  (let ((acc nil) (continue? t))
    (while continue?
      (-if-let (arg (haskell-parser--parse-arg))
          (!cons arg acc)
        (setq continue? nil))
      (haskell-parser--consume-arrow))

    (->> (nreverse acc)
         (-map 'haskell-parser--chop-birdtrack)
         (--remove (s-matches? (rx bos (* space) eos) it)))))

(defun haskell-parser--parse-arg ()
  (unless (eobp)
    (prog1 (haskell-parser--parse-arg-type)
      (haskell-parser--consume-comments)
      (haskell-parser--consume-newline-and-birdtrack))))

(defun haskell-parser--parse-arg-type ()
  (let ((start (point)))
    (haskell-parser--consume-whitespace)
    (while (not (or (eobp) (eolp)
                    (thing-at-point-looking-at (rx (or "{-" "--" "→" "->")))))
      (when (thing-at-point 'sexp)
        (forward-sexp)
        (haskell-parser--consume-whitespace)))
    (while (s-matches? (rx space) (char-to-string (char-before)))
      (forward-char -1))


    (buffer-substring start (point))))

(provide 'haskell-parser)

;;; haskell-parser.el ends here
