;;; haskell-ghc-opts.el --- Functions for working with GHC options in Haskell files.  -*- lexical-binding: t; -*-

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

(defconst haskell-ghc-opts
  '("-fcase-merge"
    "-fcse"
    "-fdefer-type-errors"
    "-fglasgow-exts"
    "-fhelpful-errors"
    "-firrefutable-tuples"
    "-fno-defer-type-errors"
    "-fno-glasgow-exts"
    "-fno-helpful-errors"
    "-fno-implicit-import-qualified"
    "-fno-irrefutable-tuples"
    "-fno-print-bind-contents"
    "-fno-warn-auto-orphans"
    "-fno-warn-deprecated-flags"
    "-fno-warn-duplicate-exports"
    "-fno-warn-hi-shadowing"
    "-fno-warn-identities"
    "-fno-warn-implicit-prelude"
    "-fno-warn-incomplete-patterns"
    "-fno-warn-incomplete-record-updates"
    "-fno-warn-incomplete-uni-patterns"
    "-fno-warn-lazy-unlifted-bindings"
    "-fno-warn-missing-fields"
    "-fno-warn-missing-local-sigs"
    "-fno-warn-missing-methods"
    "-fno-warn-missing-signatures"
    "-fno-warn-monomorphism-restriction"
    "-fno-warn-name-shadowing"
    "-fno-warn-orphans"
    "-fno-warn-overlapping-patterns"
    "-fno-warn-safe"
    "-fno-warn-tabs"
    "-fno-warn-type-defaults"
    "-fno-warn-unrecognised-pragmas"
    "-fno-warn-unsafe"
    "-fno-warn-unticked-promoted-constructors"
    "-fno-warn-unused-binds"
    "-fno-warn-unused-do-bind"
    "-fno-warn-unused-imports"
    "-fno-warn-unused-matches"
    "-fno-warn-wrong-do-bind"
    "-fno-warn-missing-import-lists"
    "-fwarn-amp"
    "-fwarn-deprecated-flags"
    "-fwarn-duplicate-constraints"
    "-fwarn-duplicate-exports"
    "-fwarn-hi-shadowing"
    "-fwarn-identities"
    "-fwarn-implicit-prelude"
    "-fwarn-incomplete-patterns"
    "-fwarn-incomplete-record-updates"
    "-fwarn-incomplete-uni-patterns"
    "-fwarn-lazy-unlifted-bindings"
    "-fwarn-missing-fields"
    "-fwarn-missing-import-lists"
    "-fwarn-missing-local-sigs"
    "-fwarn-missing-methods"
    "-fwarn-missing-signatures"
    "-fwarn-monomorphism-restriction"
    "-fwarn-name-shadowing"
    "-fwarn-orphans"
    "-fwarn-overlapping-patterns"
    "-fwarn-safe"
    "-fwarn-tabs"
    "-fwarn-type-defaults"
    "-fwarn-typed-holes"
    "-fwarn-unrecognised-pragmas"
    "-fwarn-unsafe"
    "-fwarn-unticked-promoted-constructors"
    "-fwarn-unused-binds"
    "-fwarn-unused-do-bind"
    "-fwarn-unused-imports"
    "-fwarn-unused-matches"
    "-fwarn-warnings-deprecations"
    "-fwarn-wrong-do-bind"))

(defconst haskell-ghc-opts--regex
  (rx "{-#" (+ space) "OPTIONS_GHC" (+ space)
      (group (*? nonl))
      (+ space) "#-}"))

(defun haskell-ghc-opts-in-file ()
  (->> (buffer-string)
       substring-no-properties
       (s-match-strings-all haskell-ghc-opts--regex)
       (-map #'cdr)
       (-flatten)
       (--mapcat (s-split (rx (* space) "," (* space)) it))))

;;;###autoload
(defun haskell-ghc-opts--goto-buffer-start ()
  (goto-char (point-min))

  ;; Skip #! line
  (when (and (s-matches? (rx bol "#!") (cb-buffers-current-line))
             (search-forward "#!" nil t))
    (goto-char (line-end-position))
    (forward-char 1))

  (while (and (not (eobp))
              (s-blank? (cb-buffers-current-line)))
    (forward-line 1)))

(defun haskell-ghc-opts-set (opts)
  (let ((opts (s-join " " (-sort #'string-lessp (-uniq opts)))))
    (save-excursion
      (haskell-ghc-opts-delete)
      (haskell-ghc-opts--goto-buffer-start)
      (insert (format "{-# OPTIONS_GHC %s #-}\n" opts)))))

(defun haskell-ghc-opts-delete ()
  (save-excursion
    (haskell-ghc-opts--goto-buffer-start)
    (while (search-forward-regexp haskell-ghc-opts--regex nil t)
      (replace-match "")
      (when (s-blank? (cb-buffers-current-line))
        (join-line)))))

(defun haskell-ghc-opts-insert (opt)
  "Insert OPT into the GHC options list for the current file."
  (interactive (list (completing-read "GHC Option: " haskell-ghc-opts nil t)))
  (let ((cur-opts (haskell-ghc-opts-in-file)))
    (if (--any? (s-matches? opt it) cur-opts)
        (user-error "Option %s already set" opt)
      (haskell-ghc-opts-set (cons opt cur-opts)))))

;;;###autoload
(defun haskell-ghc-opts-init ()
  (evil-define-key 'normal haskell-mode-map (kbd "SPC i o") #'haskell-ghc-opts-insert))

(provide 'haskell-ghc-opts)

;;; haskell-ghc-opts.el ends here
