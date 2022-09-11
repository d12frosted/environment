;;; lib-bg-post.el --- Utilities for Barberry Garden related articles -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 11 Sep 2022
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
;; NB! It's a piece of atrocity that helps me. DO NOT REUSE THIS CODE!
;;
;; These helpers are here to ease creation of blog posts for Barberry
;; Garden events.
;;
;; See https://barberry.io for more information about the project.
;;
;;; Code:



(defun bg-post-insert-wine ()
  "Select a wine entry and insert a heading with its information."
  (interactive)
  (let* ((wine (vulpea-select-from "Wine" (vulpea-db-query-by-tags-every '("wine" "cellar"))
                                   :require-match t)))
    ;; create heading
    (insert
     "* " (vulpea-note-title wine) "\n"
     "\n")

    ;; deal with attachment
    (let* ((image (vulpea-note-meta-get wine "images" 'link))
           (image (string-remove-prefix "attachment:" image))
           (source (expand-file-name image (org-attach-dir-from-id (vulpea-note-id wine))))
           (target-dir (org-attach-dir-from-id (org-id-get-create)))
           (target (expand-file-name image target-dir)))
      (mkdir target-dir t)
      (shell-command (format "convert '%s' -strip -auto-orient '%s'" source target))
      (insert
       "#+attr_html: :class bottle-right\n"
       "[[attachment:" image "]]\n"
       "\n"))

    ;; info
    (insert
     (bg-wine-info wine 'regular 'pick-price))))



(provide 'lib-bg-post)
;;; lib-bg-post.el ends here
