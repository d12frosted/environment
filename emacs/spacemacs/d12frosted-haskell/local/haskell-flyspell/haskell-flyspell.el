;;; haskell-flyspell.el --- Flyspell integration for Haskell.  -*- lexical-binding: t; -*-

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

(require 'flyspell)
(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")

(defun haskell-flyspell-verify ()
  "Prevent common flyspell false positives in haskell-mode."
  (and (flyspell-generic-progmode-verify)
       (not (or (s-matches? (rx bol (? ">") (* space) "{-#") (cb-buffers-current-line))
                (s-matches? (rx bol (? ">") (* space) "foreign import") (cb-buffers-current-line))))))

(defun haskell-flyspell-init ()
  (setq-local flyspell-generic-check-word-predicate #'haskell-flyspell-verify))

(provide 'haskell-flyspell)

;;; haskell-flyspell.el ends here
