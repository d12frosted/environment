;;; lib-buffer-test.el --- Tests for buffer utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; Maintainer: Boris Buliga <d12frosted@d12frosted.local>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; Created: 17 Mar 2021
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
;; This file contains tests for `lib-buffer' module.
;;
;;; Code:

(require 'buttercup)
(require 'lib-buffer)

(describe "buffer-content"
  (it "returns an empty string in empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer)))
      ;; we can get content of the buffer by name
      (expect (buffer-content name) :to-equal "")

      ;; we can get content of the buffer by object
      (expect (buffer-content buffer) :to-equal "")

      ;; current buffer is not modified
      (expect (current-buffer) :to-equal current-buffer)))

  (it "returns content of non-empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer))
           (expected "hello\nmy dear\nfrodo\n"))
      (with-current-buffer buffer
        (insert expected))

      ;; we can get content of the buffer by name
      (expect (buffer-content name) :to-equal expected)

      ;; we can get content of the buffer by object
      (expect (buffer-content buffer) :to-equal expected)

      ;; current buffer is not modified
      (expect (current-buffer) :to-equal current-buffer))))

(describe "buffer-lines"
  (it "returns an empty list in empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer)))
      ;; we can get content of the buffer by name
      (expect (buffer-lines name) :to-equal nil)

      ;; we can get lines of the buffer by object
      (expect (buffer-lines buffer) :to-equal nil)

      ;; current lines is not modified
      (expect (current-buffer) :to-equal current-buffer)))

  (it "returns all lines of non-empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer))
           (expected '("hello"
                       "my dear"
                       "frodo")))
      (with-current-buffer buffer
        (insert (string-join expected "\n")))

      ;; we can get lines of the buffer by name
      (expect (buffer-lines name) :to-equal expected)

      ;; we can get lines of the buffer by object
      (expect (buffer-lines buffer) :to-equal expected)

      ;; current buffer is not modified
      (expect (current-buffer) :to-equal current-buffer))))

(describe "buffer-lines-map"
  (it "returns an empty list in empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer)))
      ;; we can map lines of the buffer by name
      (expect (buffer-lines-map name #'length) :to-equal nil)

      ;; we can map lines of the buffer by object
      (expect (buffer-lines-map buffer #'length) :to-equal nil)

      ;; current buffer is not modified
      (expect (current-buffer) :to-equal current-buffer)))

  (it "returns all lines of non-empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer))
           (content '("hello"
                      "my dear"
                      "frodo"))
           (expected (seq-map #'length content)))
      (with-current-buffer buffer
        (insert (string-join content "\n")))

      ;; we can map lines of the buffer by name
      (expect (buffer-lines-map name #'length) :to-equal expected)

      ;; we can map lines of the buffer by object
      (expect (buffer-lines-map buffer #'length) :to-equal expected)

      ;; current buffer is not modified
      (expect (current-buffer) :to-equal current-buffer))))

(describe "buffer-lines-each-t"
  (it "does nothing in empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer)))
      (buffer-lines-each-t buffer
        (lambda (x) (format "%s :: %s" (length x) x)))

      (expect (buffer-lines name) :to-equal nil)
      (expect (buffer-lines buffer) :to-equal nil)
      (expect (current-buffer) :to-equal current-buffer)))

  (it "transforms lines in non-empty buffer"
    (let* ((current-buffer (current-buffer))
           (buffer (generate-new-buffer "test-buffer"))
           (name (buffer-name buffer))
           (content '("hello"
                      "my dear"
                      "frodo"))
           (expected (seq-map
                      (lambda (x) (format "%s :: %s" (length x) x))
                      content)))
      (with-current-buffer buffer
        (insert (string-join content "\n")))

      (buffer-lines-each-t buffer
        (lambda (x) (format "%s :: %s" (length x) x)))

      (expect (buffer-lines name) :to-equal expected)
      (expect (buffer-lines buffer) :to-equal expected)
      (expect (current-buffer) :to-equal current-buffer))))

(describe "buffer-save-modified-in-mode"
  (it "saves modified file buffers whose mode matches"
    (let ((file (make-temp-file "lib-buffer-test-")))
      (unwind-protect
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (text-mode)
                  (setq-local require-final-newline nil)
                  (insert "hello frodo")
                  (expect (buffer-modified-p) :to-be-truthy)
                  (expect (buffer-save-modified-in-mode 'text-mode)
                          :to-equal 1)
                  (expect (buffer-modified-p) :to-be nil)
                  (expect (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))
                          :to-equal "hello frodo"))
              (kill-buffer buffer)))
        (delete-file file))))

  (it "ignores modified buffers of other modes"
    (let ((file (make-temp-file "lib-buffer-test-")))
      (unwind-protect
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (fundamental-mode)
                  (insert "hello frodo")
                  (expect (buffer-save-modified-in-mode 'text-mode)
                          :to-equal 0)
                  (expect (buffer-modified-p) :to-be-truthy))
              (with-current-buffer buffer
                (set-buffer-modified-p nil))
              (kill-buffer buffer)))
        (delete-file file))))

  (it "ignores buffers not visiting a file"
    (let ((buffer (generate-new-buffer "test-buffer")))
      (unwind-protect
          (with-current-buffer buffer
            (text-mode)
            (insert "hello frodo")
            (expect (buffer-save-modified-in-mode 'text-mode)
                    :to-equal 0)
            (expect (buffer-modified-p) :to-be-truthy))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(provide 'lib-buffer-test)
;;; lib-buffer-test.el ends here
