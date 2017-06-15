;;; haskell-smart-commands.el --- Context-sensitive editing commands.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (haskell-mode "16.1-git"))

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

;; Context-sensitive editing commands for haskell-mode.

;;; Code:

(require 's)

(autoload 'evil-define-key "evil-core")
(autoload 'haskell-interactive-at-compile-message "haskell-interactive-mode")
(autoload 'haskell-interactive-mode-map "haskell-interactive-mode")
(autoload 'haskell-interactive-mode-space "haskell-interactive-mode")
(autoload 'haskell-mode-map "haskell-mode")
(autoload 'shm-map "shm")
(autoload 'sp-generic-prog-backspace "sp-generic-prog")
(autoload 'sp-generic-prog-space "sp-generic-prog")
(autoload 'thing-at-point-looking-at "thingatpt")

(defun haskell-smart-commands--after-subexpr-opening? ()
  (s-matches? (rx (or "{" "[" "{-" "{-#" "(#" "{-@") (* space) eol)
              (buffer-substring (line-beginning-position) (point))))

(defun haskell-smart-commands--before-subexp-closing? ()
  (s-matches? (rx bol (? ">") (* space) (or "}" "]" "-}" "#-}" "@-}" "#)"))
              (buffer-substring (point) (line-end-position))))

;;;###autoload
(defun haskell-smart-commands-space ()
  "Insert a space with context-sensitive formatting."
  (interactive)
  (cond
   ((and (haskell-smart-commands--after-subexpr-opening?) (haskell-smart-commands--before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (sp-generic-prog-space))))

;;;###autoload
(defun haskell-smart-commands-interactive-space ()
  "Insert a space with context-sensitive formatting."
  (interactive)
  (cond
   ((haskell-interactive-at-compile-message)
    (call-interactively #'haskell-interactive-mode-space))
   ((and (haskell-smart-commands--after-subexpr-opening?) (haskell-smart-commands--before-subexp-closing?))
    (delete-horizontal-space)
    (insert " ")
    (save-excursion (insert " ")))
   (t
    (insert " "))))

;;;###autoload
(defun haskell-smart-commands-backspace ()
  "Delete backwards with context-sensitive formatting."
  (interactive)
  (cond
   ((and (haskell-smart-commands--after-subexpr-opening?)
         (haskell-smart-commands--before-subexp-closing?)
         (thing-at-point-looking-at (rx (+ space))))
    (delete-horizontal-space))

   ((and (s-matches? (rx (or "{-#" "{-@" "{-" "(#") eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol (? ">") (or "@-}" "#-}" "-}" "#)"))
                     (buffer-substring (point) (line-end-position))))
    (delete-char 1)
    (delete-char -1))

   (t
    (sp-generic-prog-backspace))))

;;;###autoload
(defun haskell-smart-commands-init ()
  (with-eval-after-load 'shm
    (define-key (with-no-warnings shm-map) (kbd "SPC") nil))

  (with-eval-after-load 'haskell-mode
    (let ((map (with-no-warnings haskell-mode-map)))

      (define-key map (kbd "<backspace>")  #'haskell-smart-commands-backspace)
      (define-key map (kbd "SPC") #'haskell-smart-commands-space)

      (with-eval-after-load 'evil
        (evil-define-key 'insert map (kbd "SPC") #'haskell-smart-commands-space)
        (evil-define-key 'normal map (kbd "SPC") nil)
        (evil-define-key 'insert map (kbd "<backspace>") #'haskell-smart-commands-backspace)
        (evil-define-key 'normal map (kbd "<backspace>") nil))))

  (with-eval-after-load 'haskell-interactive-mode
    (let ((map (with-no-warnings haskell-interactive-mode-map)))
      (define-key map (kbd "SPC") #'haskell-smart-commands-interactive-space)
      (with-eval-after-load 'evil
        (evil-define-key 'insert map (kbd "SPC") #'haskell-smart-commands-interactive-space)
        (evil-define-key 'normal map (kbd "SPC") nil)
        (evil-define-key 'insert map (kbd "<backspace>") #'haskell-smart-commands-backspace)
        (evil-define-key 'normal map (kbd "<backspace>") nil)))))

(provide 'haskell-smart-commands)

;;; haskell-smart-commands.el ends here
