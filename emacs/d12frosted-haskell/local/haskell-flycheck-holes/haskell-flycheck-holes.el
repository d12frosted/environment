;;; haskell-flycheck-holes.el --- Strip file paths from error output.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (dash "2.12.1") (flycheck "20160604.154"))

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

;; Strip file paths from error output to make typed holes more usable.

;;; Code:

(require 'dash)
(require 'flycheck)
(require 's)

(defvar haskell-flycheck-holes-haskell-checkers '(haskell-stack-ghc haskell-ghc haskell-liquid intero)
  "List of the Flycheck checkers that will be processed.")

(defconst haskell-flycheck-holes--src-file-rx
  (rx bol (+ space) (? "(bound ") "at /" (* nonl) eol))

(defun haskell-flycheck-holes--strip-filepath (str)
  (->> (s-split "\n" str)
       (--remove (s-matches? haskell-flycheck-holes--src-file-rx it))
       (s-join "\n")))

(defun haskell-flycheck-holes--strip-files-from-errors (errors)
  (dolist (err errors)
    (let ((msg (haskell-flycheck-holes--strip-filepath (flycheck-error-message err))))
      (setf (flycheck-error-message err) msg)))
  errors)

(defun haskell-flycheck-holes--format-errors (errors)
  (-> errors
      flycheck-dedent-error-messages
      haskell-flycheck-holes--strip-files-from-errors
      flycheck-sanitize-errors))

(defun haskell-flycheck-holes-init ()
  (dolist (checker-name haskell-flycheck-holes-haskell-checkers)
    (put checker-name 'flycheck-error-filter #'haskell-flycheck-holes--format-errors)))

(provide 'haskell-flycheck-holes)

;;; haskell-flycheck-holes.el ends here
