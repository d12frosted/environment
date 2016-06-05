;;; cb-haskell-alignment.el --- Alignment configuration for Haskell-mode.  -*- lexical-binding: t; -*-

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

(require 'align)

(defconst cb-haskell-alignment-rules
  '((haskell-types
     (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-assignment
     (regexp . "\\(\\s-+\\)=\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-arrows
     (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-left-arrows
     (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
     (modes quote(haskell-mode literate-haskell-mode)))))

;;;###autoload
(defun cb-haskell-alignment-init ()
  (dolist (rule cb-haskell-alignment-rules)
    (add-to-list 'align-rules-list rule)))

(provide 'cb-haskell-alignment)

;;; cb-haskell-alignment.el ends here
