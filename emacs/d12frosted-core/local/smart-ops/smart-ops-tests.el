;;; smart-ops-tests.el --- Tests for smart-ops.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed after the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for smart-ops.el

;;; Code:

(require 'ert nil t)
(require 'smart-ops nil t)
(require 'noflet nil t)

;; Incremental insertion tests
;;
;; Test that smart ops create the expected padding during normal typing.

(defmacro define-smart-ops-incremental-test
    (name _ins fst _inserting op _run rules _insert snd _results _in expected-state)
  (declare (indent 1))
  `(ert-deftest ,(intern (format "insert-incrementally--%s" name)) ()
     (with-temp-buffer
       (smart-ops-mode)
       (insert ,fst)
       (let ((smart-ops-debug? t))
         (smart-ops-insert ,op ,rules))
       (insert ,snd)
       (should (equal (buffer-string) ,expected-state)))))

(define-smart-ops-incremental-test inserts-with-no-padding
  typing "A"
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after nil))
  typing "B"
  results in "A+B")

(define-smart-ops-incremental-test existing-padding--inserts-with-no-padding
  typing "A "
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after nil))
  typing "B"
  results in "A+B")

(define-smart-ops-incremental-test inserts-with-padding-before
  typing "A"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after nil))
  typing "B"
  results in "A +B")

(define-smart-ops-incremental-test existing-padding-inserts-with-padding-before--no-change
  typing "A "
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after nil))
  typing "B"
  results in "A +B")

(define-smart-ops-incremental-test inserts-with-padding-after
  typing "A"
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after t))
  typing "B"
  results in "A+ B")

(define-smart-ops-incremental-test inserts-with-padding-before-and-after
  typing "A"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  typing "B"
  results in "A + B")

(define-smart-ops-incremental-test uses-maximal-op-to-compute-padding-1
  typing "A+"
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after nil)
          (:op "++" :pad-before t :pad-after t))
  typing "B"
  results in "A ++ B")

(define-smart-ops-incremental-test uses-maximal-op-to-compute-padding-2
  typing "A+ "
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after nil)
          (:op "++" :pad-before t :pad-after t))
  typing "B"
  results in "A ++ B")

(define-smart-ops-incremental-test uses-maximal-op-to-compute-padding-3
  typing "A +"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t)
          (:op "++" :pad-before nil :pad-after nil))
  typing "B"
  results in "A++B")

(define-smart-ops-incremental-test uses-maximal-op-to-compute-padding-3
  typing "A + "
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t)
          (:op "++" :pad-before nil :pad-after nil))
  typing "B"
  results in "A++B")

(define-smart-ops-incremental-test calls-op-action
  typing "A"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :action (lambda (&rest _) (insert "B"))))
  typing ""
  results in "A + B")



;; Insertion tests
;;
;; Test that smart ops apply the expected padding when inserting into existing
;; text.

(defmacro define-smart-ops-insertion-test
    (name _starting _with starting-state _insert op _rules rules _results _in expected)
  (declare (indent 1))
  `(ert-deftest ,(intern (format "insert--%s" name)) ()
     (with-temp-buffer
       (lisp-mode-variables t)
       (smart-ops-mode)
       (insert ,starting-state)
       (unless (search-backward "|" nil t)
         (error "Input string must contain a pipe char to represent the insertion point."))
       (delete-char 1)
       (let ((smart-ops-debug? t))
         (smart-ops-insert ,op ,rules))
       (insert "|")
       (should (equal (buffer-string) ,expected)))))

(define-smart-ops-insertion-test inserts-with-padding-1
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A + |B")

(define-smart-ops-insertion-test inserts-with-padding-2
  starting with "A | B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A + |B")

(define-smart-ops-insertion-test inserts-with-padding-3
  starting with "A| B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A + |B")

(define-smart-ops-insertion-test inserts-with-padding-4
  starting with "A |B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A + |B")

(define-smart-ops-insertion-test pad-before--preserves-indentation
  starting with "  |A"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "  + |A")

(define-smart-ops-insertion-test no-pad-before--preserves-indentation
  starting with "  |A"
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after t))
  results in "  + |A")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-1
  starting with "A +| B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t)
          (:op "++" :pad-before t :pad-after t))
  results in "A ++ |B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-2
  starting with "A +| B"
  inserting "="
  rules '((:op "+" :pad-before t :pad-after t)
          (:op "+=" :pad-before t :pad-after t))
  results in "A += |B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-3
  starting with "A+|B"
  inserting "="
  rules '((:op "+=" :pad-before t :pad-after t)
          (:op "+" :pad-before t :pad-after t))
  results in "A += |B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-4-1
  starting with "A+|B"
  inserting "="
  rules '((:op "+=" :pad-before t :pad-after t)
          (:op "+" :pad-before nil :pad-after nil))
  results in "A += |B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-4-2
  starting with "A+|B"
  inserting "="
  rules '((:op "+" :pad-before nil :pad-after nil)
          (:op "+=" :pad-before t :pad-after t))
  results in "A += |B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-5-1
  starting with "A + |B"
  inserting "="
  rules '((:op "+" :pad-before t :pad-after t)
          (:op "+=" :pad-before nil :pad-after nil))
  results in "A+=|B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-5-2
  starting with "A + |B"
  inserting "="
  rules '((:op "+=" :pad-before nil :pad-after nil)
          (:op "+" :pad-before t :pad-after t))
  results in "A+=|B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-6-1
  starting with "A + |B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t)
          (:op "++" :pad-before nil :pad-after nil))
  results in "A++|B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-6-2
  starting with "A + |B"
  inserting "+"
  rules '((:op "++" :pad-before nil :pad-after nil)
          (:op "+" :pad-before t :pad-after t))
  results in "A++|B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-7-1
  starting with "A+|B"
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after nil)
          (:op "++" :pad-before t :pad-after t))
  results in "A ++ |B")

(define-smart-ops-insertion-test uses-exact-operator-match-for-compound-ops-7-2
  starting with "A+|B"
  inserting "+"
  rules '((:op "++" :pad-before t :pad-after t)
          (:op "+" :pad-before nil :pad-after nil))
  results in "A ++ |B")

(define-smart-ops-insertion-test uses-generic-pre-and-post-padding-for-unknown-compound-ops
  starting with "A+|B"
  inserting "="
  rules '((:op "+" :pad-before nil :pad-after nil)
          (:op "=" :pad-before t :pad-after t))
  results in "A += |B")

(define-smart-ops-insertion-test applies-pre-padding-to-compound-operators-1
  starting with "A+| B"
  inserting "+"
  rules '((:op "+" :pad-before nil :pad-after nil)
          (:op "++" :pad-before t :pad-after t))
  results in "A ++ |B")

(define-smart-ops-insertion-test applies-pre-padding-to-compound-operators-2
  starting with "A+| B"
  inserting "="
  rules '((:op "+" :pad-before nil :pad-after nil)
          (:op "=" :pad-before nil :pad-after nil))
  results in "A += |B")

(define-smart-ops-insertion-test applies-pre-padding-to-compound-operators-3
  starting with "A+| B"
  inserting "="
  rules '((:op "+=" :pad-before t :pad-after t))
  results in "A += |B")


(define-smart-ops-insertion-test applies-no-pre-padding-if-not-an-operator
  starting with "A+| C"
  inserting "B"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A+B| C")

(define-smart-ops-insertion-test applies-no-post-padding-if-not-an-operator
  starting with "A+ |C"
  inserting "B"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A+ B|C")

(define-smart-ops-insertion-test applies-pre-and-post-padding-to-compound-operators-1
  starting with "A+|B"
  inserting "+"
  rules '((:op "++" :pad-before t :pad-after t))
  results in "A ++ |B")

(define-smart-ops-insertion-test applies-pre-and-post-padding-to-compound-operators-2
  starting with "A+|B"
  inserting "+"
  rules '((:op "++" :pad-before nil :pad-after t))
  results in "A++ |B")

(define-smart-ops-insertion-test applies-pre-and-post-padding-to-compound-operators-3
  starting with "A+|B"
  inserting "+"
  rules '((:op "++" :pad-before t :pad-after nil))
  results in "A ++|B")

(define-smart-ops-insertion-test applies-pre-and-post-padding-to-compound-operators-4
  starting with "A +|B"
  inserting "+"
  rules '((:op "++" :pad-before nil :pad-after nil))
  results in "A++|B")

(define-smart-ops-insertion-test no-padding-inside-comments
  starting with "; A|B "
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "; A+|B ")

(define-smart-ops-insertion-test no-padding-inside-strings
  starting with " \"A|B\" "
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in " \"A+|B\" ")

(define-smart-ops-insertion-test no-pre-padding-at-start-of-operator-section
  starting with "(|"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "(+ |")

(define-smart-ops-insertion-test pre-padding-if-pre-padding-predicate-succeeds-1
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-before-unless (lambda (_) nil)))
  results in "A + |B")

(define-smart-ops-insertion-test pre-padding-if-pre-padding-predicate-succeeds-2
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-before-if (lambda (_) t)))
  results in "A + |B")

(define-smart-ops-insertion-test no-pre-padding-if-pre-padding-predicate-fails-1
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-before-unless (lambda (_) t)))
  results in "A+ |B")

(define-smart-ops-insertion-test no-pre-padding-if-pre-padding-predicate-fails-2
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-before-if (lambda (_) nil)))
  results in "A+ |B")

(define-smart-ops-insertion-test post-padding-if-pre-padding-predicate-succeeds-1
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-after-unless (lambda (_) nil)))
  results in "A + |B")

(define-smart-ops-insertion-test post-padding-if-pre-padding-predicate-succeeds-2
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-after-if (lambda (_) t)))
  results in "A + |B")

(define-smart-ops-insertion-test no-post-padding-if-pre-padding-predicate-fails-1
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-after-unless (lambda (_) t)))
  results in "A +|B")

(define-smart-ops-insertion-test no-post-padding-if-pre-padding-predicate-fails-2
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-after-if (lambda (_) nil)))
  results in "A +|B")

(define-smart-ops-insertion-test pre-and-post-padding-if-general-padding-predicate-succeeds
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-unless (lambda (_) nil)))
  results in "A + |B")

(define-smart-ops-insertion-test no-pre-or-post-padding-if-general-padding-predicate-fails-1
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-unless (lambda (_) t)))
  results in "A+|B")

(define-smart-ops-insertion-test no-pre-or-post-padding-if-general-padding-predicate-fails-2
  starting with "A|B"
  inserting "+"
  rules '((:op "+" :pad-before t :pad-after t :pad-if (lambda (_) nil)))
  results in "A+|B")

(define-smart-ops-insertion-test insertion-into-defined-ops-1
  starting with "<|>"
  inserting "A"
  rules '((:op "<" :pad-before t :pad-after t)
          (:op ">" :pad-before t :pad-after t)
          (:op "<>" :pad-before nil :pad-after nil))
  results in "<A|>")

(define-smart-ops-insertion-test insertion-into-defined-ops-2
  starting with "<A|>"
  inserting "B"
  rules '((:op "<" :pad-before t :pad-after t)
          (:op ">" :pad-before t :pad-after t)
          (:op "<>" :pad-before nil :pad-after nil))
  results in "<AB|>")

(define-smart-ops-insertion-test insertion-into-defined-ops-3
  starting with "A<B|>"
  inserting "C"
  rules '((:op "<" :pad-before t :pad-after t)
          (:op ">" :pad-before t :pad-after t)
          (:op "<>" :pad-before nil :pad-after nil))
  results in "A<BC|>")

(define-smart-ops-insertion-test insertion-into-defined-ops-4
  starting with "A<B|>D"
  inserting "C"
  rules '((:op "<" :pad-before t :pad-after t)
          (:op ">" :pad-before t :pad-after t)
          (:op "<>" :pad-before nil :pad-after nil))
  results in "A<BC|>D")

(define-smart-ops-insertion-test pad-before--after-match-true--no-pre-pad
  starting with "foo|"
  inserting "+"
  rules `((:op "+" :pad-before-unless ,(smart-ops-after-match? (rx "foo" eos))))
  results in "foo+ |")

(define-smart-ops-insertion-test pad-before--after-match-false--applies-pre-pad
  starting with "foo|"
  inserting "+"
  rules `((:op "+" :pad-before-unless ,(smart-ops-after-match? (rx "bar" eos))))
  results in "foo + |")

(define-smart-ops-insertion-test no-pad-before--after-match-true--no-pre-pad
  starting with "foo|"
  inserting "+"
  rules `((:op "+" :pad-before nil :pad-before-unless ,(smart-ops-after-match? (rx "foo" eos))))
  results in "foo+ |")

(define-smart-ops-insertion-test no-pad-before--after-match-false--no-pre-pad
  starting with "foo|"
  inserting "+"
  rules `((:op "+" :pad-before nil :pad-before-unless ,(smart-ops-after-match? (rx "bar" eos))))
  results in "foo+ |")

(define-smart-ops-insertion-test pad-after--before-match-true--no-post-pad
  starting with "foo|bar"
  inserting "+"
  rules `((:op "+" :pad-after-unless ,(smart-ops-before-match? (rx "bar" eos))))
  results in "foo +|bar")

(define-smart-ops-insertion-test pad-after--before-match-false--applies-post-pad
  starting with "foo|bar"
  inserting "+"
  rules `((:op "+" :pad-after-unless ,(smart-ops-before-match? (rx "baz" eos))))
  results in "foo + |bar")

(define-smart-ops-insertion-test no-pad-after--before-match-true--no-post-pad
  starting with "foo|bar"
  inserting "+"
  rules `((:op "+" :pad-after nil :pad-after-unless ,(smart-ops-before-match? (rx "bar" eos))))
  results in "foo +|bar")

(define-smart-ops-insertion-test no-pad-after--before-match--false--no-post-pad
  starting with "foo|bar"
  inserting "+"
  rules `((:op "+" :pad-after nil :pad-after-unless ,(smart-ops-before-match? (rx "baz" eos))))
  results in "foo +|bar")

(define-smart-ops-insertion-test preserves-location-of-pt-for-compound-ops-1
  starting with "foo|+bar"
  inserting "+"
  rules `((:op "+" :pad-after nil :pad-before nil)
          (:op "++" :pad-after t :pad-before t))
  results in "foo +|+ bar")

(define-smart-ops-insertion-test preserves-location-of-pt-for-compound-ops-2
  starting with "foo|=bar"
  inserting "+"
  rules `((:op "+" :pad-after t :pad-before t)
          (:op "=" :pad-after t :pad-before t)
          (:op "+=" :pad-after nil :pad-before nil))
  results in "foo+|=bar")

(define-smart-ops-insertion-test preserves-padding-before-semicolon-1
  starting with "foo|;"
  inserting "+"
  rules `((:op ";" :pad-after nil :pad-before nil)
          (:op "+" :pad-after t :pad-before t))
  results in "foo + |;")

(define-smart-ops-insertion-test preserves-padding-before-semicolon-2
  starting with "foo|;"
  inserting "+"
  rules `((:op ";" :pad-after t :pad-before t)
          (:op "+" :pad-after t :pad-before t))
  results in "foo + |;")


(define-smart-ops-insertion-test no-special-action-when-bypassed-1
  starting with "foo|bar"
  inserting "+"
  rules `((:op "+" :bypass? t))
  results in "foo+|bar")

(define-smart-ops-insertion-test no-special-action-when-bypassed-2
  starting with "foo| bar"
  inserting "+"
  rules `((:op "+" :bypass? t))
  results in "foo+| bar")



;; Deletion tests
;;
;; Test that backspace command deletes operators and padding.

(defmacro define-smart-ops-backspace-test
    (name _deleting _at starting-state _rules rules _results _in expected)
  (declare (indent 1))
  `(ert-deftest ,(intern (format "backspace--%s" name)) ()
     (with-temp-buffer
       (lisp-mode-variables t)
       (smart-ops-mode)
       (insert ,starting-state)
       (unless (search-backward "|" nil t)
         (error "Input string must contain a pipe char to represent the insertion point."))
       (delete-char 1)
       (noflet ((smart-ops--rules-for-current-mode () ,rules))
         (smart-ops-backspace))
       (insert "|")
       (should (equal (buffer-string) ,expected)))))

(define-smart-ops-backspace-test deletes-normal-character
  deleting at "A|"
  rules ()
  results in "|")

(define-smart-ops-backspace-test deletes-normal-space-1
  deleting at "A |"
  rules ()
  results in "A|")

(define-smart-ops-backspace-test deletes-normal-space-1
  deleting at "A  |"
  rules ()
  results in "A |")

(define-smart-ops-backspace-test deletes-smart-op-no-pad
  deleting at "A+|"
  rules '((:op "+"))
  results in "A|")

(define-smart-ops-backspace-test deletes-through-padding
  deleting at "A + |"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A|")

(define-smart-ops-backspace-test deletes-through-padding-to-prev-op
  deleting at "A ++ |"
  rules '((:op "+" :pad-before t :pad-after t))
  results in "A +|")



;; Utilities

(ert-deftest parameter-validation--sets-expected-defaults ()
  (let* ((defaults (list :bypass? nil
                         :pad-before t
                         :pad-after t
                         :action #'ignore
                         :pad-before-if smart-ops--true-fn
                         :pad-before-unless smart-ops--nil-fn
                         :pad-after-if smart-ops--true-fn
                         :pad-after-unless smart-ops--nil-fn
                         :pad-if smart-ops--true-fn
                         :pad-unless smart-ops--nil-fn))

         (updated (smart-ops--apply-defaults-for-absent-keys nil))
         (default-pairs (-partition 2 defaults))
         (updated-pairs (-partition 2 updated)))
    (should (-same-items? updated-pairs default-pairs))))

(ert-deftest parameter-validation--does-not-override-supplied-values-1 ()
  (let* ((input
          (list :bypass? nil
                :pad-before t
                :pad-after t
                :pad-if 'identity
                :pad-unless 'identity
                :pad-before-if 'identity
                :pad-before-unless 'identity
                :pad-after-if 'identity
                :pad-after-unless 'identity
                :action 'identity))
         (updated (smart-ops--apply-defaults-for-absent-keys input))
         (input-pairs (-partition 2 input))
         (updated-pairs (-partition 2 updated)))
    (should (-same-items? input-pairs updated-pairs))))

(ert-deftest parameter-validation--does-not-override-supplied-values-2 ()
  (let* ((input
          (list :bypass? t
                :pad-before nil
                :pad-after nil
                :pad-if 'identity
                :pad-unless 'identity
                :pad-before-if 'identity
                :pad-before-unless 'identity
                :pad-after-if 'identity
                :pad-after-unless 'identity
                :action 'identity
                :action 'not))
         (updated (smart-ops--apply-defaults-for-absent-keys input))
         (input-pairs (-partition 2 input))
         (updated-pairs (-partition 2 updated)))
    (should (-same-items? input-pairs updated-pairs))))

(ert-deftest after-match--matches--return-true ()
  (with-temp-buffer
    (insert "foo+")
    (skip-chars-backward "+")
    (should (funcall (smart-ops-after-match? (rx bow "foo" eos))
                     (point)))))

(ert-deftest after-match--no-match--return-false ()
  (with-temp-buffer
    (insert "foo+")
    (skip-chars-backward "+")
    (should (not (funcall (smart-ops-after-match? (rx bow "bar" eos))
                          (point))))))

(ert-deftest before-match--matches--return-true ()
  (with-temp-buffer
    (insert "+foo")
    (skip-chars-backward "foo")
    (should (funcall (smart-ops-before-match? (rx bos "foo" eow))
                     (point)))))

(ert-deftest before-match--no-match--return-false ()
  (with-temp-buffer
    (insert "+foo")
    (skip-chars-backward "foo")
    (should (not (funcall (smart-ops-before-match? (rx bos "bar" eow))
                          (point))))))

(provide 'smart-ops-tests)

;;; smart-ops-tests.el ends here
