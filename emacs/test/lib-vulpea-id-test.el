;;; lib-vulpea-id-test.el --- Tests for ID utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 07 Jul 2026
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
;; This file contains tests for `lib-vulpea-id' module.
;;
;;; Code:

(require 'buttercup)
(require 'lib-buffer)
(require 'lib-vulpea-id)

(defun lib-vulpea-id-test--with-org-file (content fn)
  "Call FN in an `org-mode' buffer visiting a temp file with CONTENT."
  (let ((file (make-temp-file "lib-vulpea-id-test-" nil ".org")))
    (unwind-protect
        (let ((buffer (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buffer
                (insert content)
                (org-mode)
                (funcall fn))
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer)))
      (delete-file file))))

(describe "vulpea-id-auto-assign"
  (it "does nothing when there are no targets"
    (lib-vulpea-id-test--with-org-file "* one\n"
     (lambda ()
       (let ((vulpea-id-auto-targets nil))
         (vulpea-id-auto-assign))
       (expect (buffer-string) :to-equal "* one\n"))))

  (it "assigns file-level ID when file is a target"
    (lib-vulpea-id-test--with-org-file "* one\n"
     (lambda ()
       (let ((vulpea-id-auto-targets '(file)))
         (vulpea-id-auto-assign))
       (goto-char (point-min))
       (expect (org-entry-get nil "ID") :to-be-truthy))))

  (it "assigns ID to every heading when headings is a target"
    (lib-vulpea-id-test--with-org-file "* one\n* two\n"
     (lambda ()
       (let ((vulpea-id-auto-targets '(headings)))
         (vulpea-id-auto-assign))
       (goto-char (point-min))
       (search-forward "* one")
       (expect (org-entry-get nil "ID") :to-be-truthy)
       (search-forward "* two")
       (expect (org-entry-get nil "ID") :to-be-truthy))))

  (it "preserves narrowing"
    (lib-vulpea-id-test--with-org-file "* one\n* two\n"
     (lambda ()
       (goto-char (point-min))
       (search-forward "* two")
       (org-narrow-to-subtree)
       (let ((vulpea-id-auto-targets '(file headings)))
         (vulpea-id-auto-assign))
       (expect (buffer-narrowed-p) :to-be-truthy)
       ;; ids are still assigned outside of the narrowed region
       (save-restriction
         (widen)
         (goto-char (point-min))
         (expect (org-entry-get nil "ID") :to-be-truthy)))))

  (it "does nothing when buffer-save-inhibit-mutations is non-nil"
    (lib-vulpea-id-test--with-org-file "* one\n"
     (lambda ()
       (let ((vulpea-id-auto-targets '(file headings))
             (buffer-save-inhibit-mutations t))
         (vulpea-id-auto-assign))
       (expect (buffer-string) :to-equal "* one\n")))))

(provide 'lib-vulpea-id-test)
;;; lib-vulpea-id-test.el ends here
