;;; lib-vulpea-buffer-prop.el --- Utilities to manipulate buffer prop -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2021 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 14 Feb 2021
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Missing utilities for buffer-wide properties manipulation.
;;
;;   #+PROP: VALUE
;;
;;; Code:

;;;###autoload
(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

;;;###autoload
(defun vulpea-buffer-prop-set (name value)
  "Set a buffer property called NAME to VALUE."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (if (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                           (point-max) t)
        (replace-match (concat "#+" name ": " value) 'fixedcase)
      ;; find the first line that doesn't begin with ':' or '#'
      (let ((found))
        (while (not (or found (eobp)))
          (beginning-of-line)
          (if (or (looking-at "^#")
                  (looking-at "^:"))
              (line-move 1 t)
            (setq found t)))
        (insert "#+" name ": " value "\n")))))

;;;###autoload
(defun vulpea-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS."
  (split-string (vulpea-buffer-prop-get name) separators))

(provide 'lib-vulpea-buffer-prop)
;;; lib-vulpea-buffer-prop.el ends here
