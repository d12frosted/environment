;;; haskell-imports.el --- Commands for inserting imports in Haskell files.  -*- lexical-binding: t; -*-

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
(autoload 'haskell-navigate-imports-go "haskell-navigate-imports")
(autoload 'haskell-session-all-modules "haskell-modules")
(autoload 'haskell-session-maybe "haskell-session")

(defun haskell-imports--insert-at-imports (str)
  "Prepend STR to this buffer's list of imported modules."
  (save-excursion
    (haskell-navigate-imports-go)
    (open-line 1)
    (insert str)))

(defun haskell-imports--module-to-qualified-name (module)
  "Make a reasonable name for MODULE for use in a qualified import."
  (-last-item (s-split (rx ".") module)))

(defun haskell-imports--exposed-modules (pkg-entry)
  (with-temp-buffer
    (insert pkg-entry)
    (goto-char (point-min))
    (when (search-forward-regexp (rx bol "exposed-modules:") nil t)
      (forward-line 1)
      (let ((start (line-beginning-position)))
        (while (s-matches? (rx bol (+ space)) (buffer-substring (line-beginning-position)(line-end-position)))
          (forward-line 1))

        (let ((section (buffer-substring-no-properties start (line-beginning-position))))
          (s-split (rx (any " \n")) section t))))))

(defun haskell-imports--package-cache-outdated? ()
  (-when-let ((line-1) (s-lines (shell-command-to-string "ghc-pkg dump")))
    (s-starts-with? "WARNING: cache is out of date" line-1)))

(defun haskell-imports--maybe-recache-packages ()
  (when (and (haskell-imports--package-cache-outdated?)
             (y-or-n-p "Package cache is outdated.  Update cache? "))
    (message "Updating...")
    (shell-command "ghc-pkg recache")))

(defun haskell-imports--module-listing ()
  "Get a list of all Haskell modules known to the current project or GHC."
  (haskell-imports--maybe-recache-packages)
  (let ((cached-packages (->> (shell-command-to-string "ghc-pkg dump")
                              (s-split "---")
                              (-mapcat #'haskell-imports--exposed-modules))))
    (-if-let (session (haskell-session-maybe))
        (-union cached-packages (haskell-session-all-modules session t))
      cached-packages)))

;;;###autoload
(defun haskell-imports-insert-qualified (module name)
  "Insert a qualified Haskell import statement for MODULE with short NAME."
  (interactive
   (let ((m (completing-read "Module: " (haskell-imports--module-listing))))
     (list m (s-trim (read-string "As: " (haskell-imports--module-to-qualified-name m)
                                  t)))))

  (if (s-matches? (rx-to-string `(and "import" (+ space) "qualified" (+ space)
                                      ,module (or space eol)))
                  (buffer-string))
      (when (called-interactively-p nil)
        (message "Module '%s' is already imported" module))

    (haskell-imports--insert-at-imports (format "import qualified %s as %s" module name))))

;;;###autoload
(defun haskell-imports-insert-unqualified (module)
  "Interactively insert a Haskell import statement for MODULE."
  (interactive (list (completing-read "Module: " (haskell-imports--module-listing))))

  (if (s-matches? (rx-to-string `(and "import" (+ space) ,module (or space eol)))
                  (buffer-string))
      (when (called-interactively-p nil)
        (message "Module '%s' is already imported" module))

    (haskell-imports--insert-at-imports (format "import %s" module))))

;;;###autoload
(defun haskell-imports-init ()
  (when (fboundp 'evil-define-key)
    (evil-define-key 'normal haskell-mode-map
      (kbd "SPC i i") #'haskell-imports-insert-unqualified
      (kbd "SPC i q") #'haskell-imports-insert-qualified))

  (when (fboundp 'spacemacs/set-leader-keys-for-major-mode)
    (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
      "ii" #'haskell-imports-insert-unqualified
      "iq" #'haskell-imports-insert-qualified)))

(provide 'haskell-imports)

;;; haskell-imports.el ends here
