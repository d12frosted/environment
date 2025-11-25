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

(require 'brb-event)
(require 'lib-brb)



;;;###autoload
(defun brb-post-insert-wine ()
  "Select a wine entry and insert a code block for it."
  (interactive)
  (when-let* ((event (brb-event-select))
              (wine (vulpea-select-from
                     "Wine" (brb-event-wines event)
                     :require-match t)))
    (insert "* " (vulpea-note-title wine) "\n\n")
    (insert "#+begin_src wine\n"
            "id: " (vulpea-note-id wine) "\n"
            "#+end_src\n")
    (funcall #'brb-post-insert-rating t)))

;;;###autoload
(defun brb-post-insert-rating (&optional extra-new-line)
  "Select a wine entry and insert a code block for it.

Inserts extra new line when EXTRA-NEW-LINE is non-nil."
  (interactive)
  (when-let* ((event (brb-event-select))
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
                                       date (vulpea-note-title wine))))))
    (when extra-new-line
      (insert "\n"))
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



(provide 'lib-brb-post)
;;; lib-brb-post.el ends here
