;;; lib-org-emphasis.el --- De-emphasize org markup characters -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 25 Nov 2025
;;
;; URL: https://github.com/d12frosted/environment/tree/master/emacs
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
;; Grey out emphasis markers (*bold*, /italic/, =verbatim=, etc.)
;; without hiding them completely. Uses org's own regexes for proper
;; pre/post context handling.
;;
;;; Code:

(require 'org)

(defgroup org-emphasis-marker nil
  "De-emphasize org markup characters."
  :group 'org)

(defface org-emphasis-marker
  '((t :inherit org-drawer))
  "Face for de-emphasized markup characters in org-mode."
  :group 'org-emphasis-marker)

(defun org-emphasis-marker--fontify (limit)
  "Fontify emphasis markers up to LIMIT using org's own regex."
  (when (and (derived-mode-p 'org-mode)
             (re-search-forward org-emph-re limit t))
    (let ((opening (match-beginning 2))
          (closing (1- (match-end 2))))
      (put-text-property opening (1+ opening) 'face 'org-emphasis-marker)
      (put-text-property closing (1+ closing) 'face 'org-emphasis-marker))
    t))

(defun org-emphasis-marker--fontify-verbatim (limit)
  "Fontify verbatim/code markers up to LIMIT."
  (when (and (derived-mode-p 'org-mode)
             (re-search-forward org-verbatim-re limit t))
    (let ((opening (match-beginning 2))
          (closing (1- (match-end 2))))
      (put-text-property opening (1+ opening) 'face 'org-emphasis-marker)
      (put-text-property closing (1+ closing) 'face 'org-emphasis-marker))
    t))

(defvar org-emphasis-marker--keywords
  '((org-emphasis-marker--fontify)
    (org-emphasis-marker--fontify-verbatim))
  "Font-lock keywords for `org-emphasis-marker-mode'.")

;;;###autoload
(define-minor-mode org-emphasis-marker-mode
  "Grey out emphasis markers without hiding them."
  :lighter ""
  :group 'org-emphasis-marker
  (if org-emphasis-marker-mode
      (progn
        (font-lock-add-keywords nil org-emphasis-marker--keywords 'append)
        (font-lock-flush))
    (font-lock-remove-keywords nil org-emphasis-marker--keywords)
    (font-lock-flush)))

(provide 'lib-org-emphasis)
;;; lib-org-emphasis.el ends here
