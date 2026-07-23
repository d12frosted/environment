;;; lib-vulpea-capture-test.el --- Tests for capture utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Jul 2026
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
;; This file contains tests for `lib-vulpea-capture' module.
;;
;;; Code:

(require 'buttercup)
(require 'vulpea-journal)
(require 'lib-vulpea-capture)

(defun lib-vulpea-capture-test--day (time)
  "Format TIME as a plain day string, or return nil when TIME is nil."
  (when time
    (format-time-string "%Y-%m-%d" time)))

(describe "vulpea-capture-journal"
  :var (date-during-capture)

  (before-each
    (setq date-during-capture 'unset)
    ;; the target function runs inside `org-capture', so the date has to
    ;; be observed from there rather than after the command returns
    (spy-on 'org-capture :and-call-fake
            (lambda (&rest _)
              (setq date-during-capture vulpea-capture--journal-date))))

  (it "captures into today without prefix argument"
    (let ((current-prefix-arg nil))
      (vulpea-capture-journal))
    (expect 'org-capture :to-have-been-called-with nil "j")
    (expect date-during-capture :to-be nil))

  (it "captures into the selected date with prefix argument"
    (spy-on 'org-read-date :and-return-value "2026-07-20")
    (let ((current-prefix-arg '(4)))
      (vulpea-capture-journal))
    (expect 'org-capture :to-have-been-called-with nil "j")
    (expect (lib-vulpea-capture-test--day date-during-capture)
            :to-equal "2026-07-20"))

  (it "does not prefer future dates when reading the date"
    (spy-on 'org-read-date :and-call-fake
            (lambda (&rest _)
              (expect org-read-date-prefer-future :to-be nil)
              "2026-07-20"))
    (let ((current-prefix-arg '(4)))
      (vulpea-capture-journal))
    (expect 'org-read-date :to-have-been-called))

  (it "leaves no date behind after capture"
    (spy-on 'org-read-date :and-return-value "2026-07-20")
    (let ((current-prefix-arg '(4)))
      (vulpea-capture-journal))
    (expect vulpea-capture--journal-date :to-be nil)))

(describe "vulpea-capture--journal-target"
  (before-each
    (spy-on 'vulpea-journal-capture-target))

  (it "targets today when no date is set"
    (let ((vulpea-capture--journal-date nil))
      (vulpea-capture--journal-target))
    (expect 'vulpea-journal-capture-target :to-have-been-called-with nil))

  (it "targets the requested date"
    (let ((vulpea-capture--journal-date
           (org-time-string-to-time "2026-07-20")))
      (vulpea-capture--journal-target))
    (expect (lib-vulpea-capture-test--day
             (car (spy-calls-args-for 'vulpea-journal-capture-target 0)))
            :to-equal "2026-07-20")))

(provide 'lib-vulpea-capture-test)
;;; lib-vulpea-capture-test.el ends here
