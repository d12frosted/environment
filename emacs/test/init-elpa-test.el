;;; init-elpa-test.el --- Tests for package setup -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 13 Oct 2026
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
;; This file contains tests for `init-elpa' module.
;;
;;; Code:

(require 'buttercup)
(require 'init-elpa)

(describe "elpa--drop-compile-time-forms"
  (it "drops compile time forms from preface when Elpaca handles the package"
    (let ((rest '(:preface ((eval-when-compile (load "foo" nil t))
                            (setq foo-prefix "C-c f"))
                  :defer t)))
      (expect (elpa--drop-compile-time-forms
               (lambda (_name _keyword _ensure rest _state) rest)
               'foo :ensure t rest nil)
              :to-equal '(:preface ((setq foo-prefix "C-c f")) :defer t))
      ;; original declaration is left intact
      (expect (length (plist-get rest :preface)) :to-equal 2)))

  (it "keeps compile time forms when Elpaca does not handle the package"
    (let ((rest '(:preface ((eval-when-compile (load "foo" nil t))) :defer t)))
      (expect (elpa--drop-compile-time-forms
               (lambda (_name _keyword _ensure rest _state) rest)
               'foo :ensure nil rest nil)
              :to-equal rest)))

  (it "does not add preface when there is none"
    (expect (elpa--drop-compile-time-forms
             (lambda (_name _keyword _ensure rest _state) rest)
             'foo :ensure t '(:defer t) nil)
            :to-equal '(:defer t))))

(provide 'init-elpa-test)
;;; init-elpa-test.el ends here
