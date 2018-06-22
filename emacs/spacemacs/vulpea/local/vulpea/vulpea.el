;;; vulpea.el --- Collection of note taking helpers -*- lexical-binding: t -*-

;; Author: Boris Buliga
;; Maintainer: Boris Buliga
;; Version: 0.1
;; Package-Requires: ((org-mode "9.1.0") (org-brain "0.5"))
;; Homepage: https://github.com/d12frosted/environment/tree/master/emacs/spacemacs/vulpea/local/vulpea
;; Keywords: personal


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(defun vulpea/insert-brain-link ()
  "Insert a link to brain entry."
  (interactive)
  (insert (vulpea--make-brain-link (vulpea--choose-brain-entry))))

(defvar vulpea-places-config
  '(("country" . "EC4A5BD7-71C4-479A-8BBB-8F022E78F52D")))

(defun vulpea/set-place ()
  "Set place properties to org entry at point."
  (interactive)
  (seq-each
   (lambda (cfg)
     (unless (org-entry-get nil (car cfg))
       (org-set-property (upcase (car cfg)) (vulpea--make-brain-link (vulpea--choose-brain-entry-by-parent (cdr cfg))))))
   vulpea-places-config))

(defun vulpea--choose-brain-entry ()
  "Choose a brain entry."
  (org-brain-choose-entry
   "Entry: "
   (append (org-brain-files t) (org-brain-headline-entries))
   nil
   t))

(defun vulpea--choose-brain-entry-by-parent (parent-id)
  "Choose a brain entry from children of PARENT-ID."
  (org-brain-choose-entry
   "Entry: "
   (org-brain-children (org-brain-entry-from-id parent-id))
   nil
   t))

(defun vulpea--make-brain-link (entry)
  "Make an org-mode link to ENTRY."
  (org-make-link-string
   (concat "brain:"
           (org-brain-entry-identifier entry))
   (org-brain-title entry)))

(provide 'vulpea)

;;; vulpea.el ends here
