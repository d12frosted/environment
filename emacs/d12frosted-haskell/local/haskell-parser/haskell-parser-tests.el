;;; haskell-parser-tests.el --- Tests for Haskell parser.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for Haskell parser.

;;; Code:

(require 'ert)
(require 'dash)
(require 'haskell-parser nil t)

(ert-deftest haskell-parser-test--parse-single-line-typesig-no-args ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism :: Doge (Cate Mus)")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Doge (Cate Mus)" ret))
      (should (null args)))))

(ert-deftest haskell-parser-test--parse-single-line-typesig-no-args-lhs ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "> dogeomorphism :: Doge (Cate Mus)")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Doge (Cate Mus)" ret))
      (should (null args)))))

(ert-deftest haskell-parser-test--parse-single-line-typesig-forall-no-args ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism :: forall c. Doge (Cate Mus)")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Doge (Cate Mus)" ret))
      (should (null args)))))

(ert-deftest haskell-parser-test--parse-single-line-typesig-unicode-forall-no-args ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism :: ∀ c. Doge (Cate Mus)")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Doge (Cate Mus)" ret))
      (should (null args)))))

(ert-deftest haskell-parser-test--parse-multiline-typesig ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ Doge
                                 → Cate
                                 → Mus")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Mus" ret))
      (should (equal '("Doge" "Cate")
                     args)))))

(ert-deftest haskell-parser-test--parse-multiline-typesig-lhs ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "
> dogeomorphism ∷ Doge
>               → Cate
>               → Mus")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Mus" ret))
      (should (equal '("Doge" "Cate")
                     args)))))

(ert-deftest haskell-parser-test--discard-comments-in-typesig1 ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ Doge → Cate → Mus -- so typesig")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Mus" ret))
      (should (equal '("Doge" "Cate")
                     args)))))

(ert-deftest haskell-parser-test--discard-comments-in-typesig2 ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ Doge {- very complex -} → Cate → Mus")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Mus" ret))
      (should (equal '("Doge" "Cate")
                     args)))))

(ert-deftest haskell-parser-test--discard-comments-in-typesig1 ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ Doge → Cate → Mus --^ so typesig")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Mus" ret))
      (should (equal '("Doge" "Cate")
                     args)))))

(ert-deftest haskell-parser-test--parse-single-line-typesig ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ Doge (Cate Mus) → (Doge → Cate)")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "(Doge → Cate)" ret))
      (should (equal '("Doge (Cate Mus)") args)))))

(ert-deftest haskell-parser-test--parse-single-line-typesig-with-constraint ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ Cate d ⇒ Doge → (Doge → d)")))
    (-let [(&plist :constraints cs :return-type ret :args args) parsed]
      (should (equal '("Cate d") cs))
      (should (equal "(Doge → d)" ret))
      (should (equal '("Doge") args)))))

(ert-deftest haskell-parser-test--parse-single-line-typesig-with-quantified-variable ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ forall d. Cate d ⇒ Doge → (Doge → d)")))
    (-let [(&plist :forall forall :constraints cs :return-type ret :args args) parsed]
      (should (equal '("d") forall))
      (should (equal '("Cate d") cs))
      (should (equal "(Doge → d)" ret))
      (should (equal '("Doge") args)))))


(ert-deftest haskell-parser-test--parse-single-line-typesig-with-multiple-constraints ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ (Cate c, Doge d) ⇒ c → (d → c)")))
    (-let [(&plist :constraints cs :return-type ret :args args) parsed]
      (should (equal '("Cate c" "Doge d") cs))
      (should (equal "(d → c)" ret))
      (should (equal '("c") args)))))

(ert-deftest haskell-parser-test--parse-complex-typesig ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "dogeomorphism ∷ Doge (Cate Mus)
                                 → (Bar → Baz) -- much args
                                 -- ^ hey dawg
                                 → Doge (Bar, Baz)
                                 {- → Doge (Mus, Cate) -}
                                 → Foo")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Foo" ret))
      (should (equal '("Doge (Cate Mus)" "(Bar → Baz)" "Doge (Bar, Baz)")
                     args)))))

(ert-deftest haskell-parser-test--parse-complex-typesig-lhs ()
  (let ((parsed  (haskell-parser-parse-typesig
                  "
> dogeomorphism ∷ Doge (Cate Mus)
>               → (Bar → Baz) -- much args
>               -- ^ hey dawg
>               → Doge (Bar, Baz)
>               {- → Doge (Mus, Cate) -}
>               → Foo")))
    (-let [(&plist :return-type ret :args args) parsed]
      (should (equal "Foo" ret))
      (should (equal '("Doge (Cate Mus)" "(Bar → Baz)" "Doge (Bar, Baz)")
                     args)))))


(provide 'haskell-parser-tests)

;;; haskell-parser-tests.el ends here
