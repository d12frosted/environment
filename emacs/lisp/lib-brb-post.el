;;; lib-brb-post.el --- Utilities for Barberry Garden related articles -*- lexical-binding: t; -*-
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

(require 'lib-brb)



;;;###autoload
(defun brb-post-insert-wine ()
  "Select a wine entry and insert a code block for it."
  (interactive)
  (when-let* ((event (brb-event-in-buffer))
              (wine (vulpea-select-from
                     "Wine" (brb-event-wines event)
                     :require-match t)))
    (insert "* " (vulpea-note-title wine) "\n\n")
    (insert "#+begin_src wine\n"
            "id: " (vulpea-note-id wine) "\n"
            "#+end_src\n")))

;;;###autoload
(defun brb-post-insert-rating ()
  "Select a wine entry and insert a code block for it."
  (interactive)
  (when-let* ((event (brb-event-in-buffer))
              (date (brb-event-date-string event))
              (wine (brb-post-get-wine-block-id-element))
              (wine (vulpea-db-get-by-id wine))
              (ratings (->> (vulpea-note-meta-get-list wine "ratings" 'note)
                            (--filter (string-equal date (vulpea-note-meta-get it "date")))))
              (rating (pcase (length ratings)
                        (0 (user-error "Could not find ratings on '%s' for '%s'"
                                       date (vulpea-note-title wine)))
                        (1 (car ratings))
                        (_ (user-error "Too many ratings on '%s' for '%s'"
                                       datetime (vulpea-note-title wine))))))
    (insert "#+begin_src review\n"
            "id: " (vulpea-note-id rating) "\n"
            "#+end_src\n")))

(defun brb-post-get-wine-block-id-element ()
  "Get the ID from wine source block using org-element API.

This is a more robust implementation using Org's element parser."
  (interactive)
  (let ((tree (org-element-parse-buffer))
        (pos (point))
        wine-id)
    ;; Find current headline
    (org-element-map tree 'headline
      (lambda (headline)
        (let ((begin (org-element-property :begin headline))
              (end (org-element-property :end headline)))
          (when (and (>= pos begin) (< pos end))
            ;; We're inside this headline, look for wine blocks
            (org-element-map headline 'src-block
              (lambda (src-block)
                (when (string= (org-element-property :language src-block) "wine")
                  ;; Parse the content for id
                  (let ((content (org-element-property :value src-block)))
                    (when (string-match "id:[ \t]*\\([^ \t\n]+\\)" content)
                      (setq wine-id (match-string 1 content))))))))))
      nil t)
    wine-id))

;; * Deprecated functions
;;

;;;###autoload
(defun brb-post-insert-wine-v0 ()
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
     (brb-wine-info wine 'regular 'pick-price))))



(provide 'lib-brb-post)
;;; lib-brb-post.el ends here
