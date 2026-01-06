;;; lib-string-test.el --- `lib-string' tests   -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 14 Apr 2022
;;
;; URL: https://github.com/d12frosted/
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
;; This file contains tests for `lib-string'.
;;
;;; Code:

(require 'buttercup)
(require 'vulpea)
(require 'lib-string)

(describe "string-from"
  (it "should support strings"
    (expect
     (string-from "some string")
     :to-equal
     "some string"))

  (it "should support integers"
    (expect
     (string-from 42)
     :to-equal
     "42"))

  (it "should support floats"
    (expect
     (string-from 121.9831000000001)
     :to-equal
     "121.9831000000001"))

  (it "should support symbols starting with '"
    (expect
     (string-from 'red)
     :to-equal
     "red"))

  (it "should support symbols starting with :"
    (expect
     (string-from :yellow)
     :to-equal
     ":yellow"))

  (it "should support vulpea notes"
    (expect
     (string-from (make-vulpea-note
                   :id (org-id-new)
                   :title "some note"))
     :to-equal
     "some note")))

(describe "string-table"
  (it "should have sane defaults"
    (expect
     (string-table
      :data '(("name" 12 :brown)
              ("short name" 192 :yellow)
              ("abnormally long name" 8 :red)))
     :to-equal
     (string-join
      '("                name  12  :brown"
        "          short name 192 :yellow"
        "abnormally long name   8    :red")
      "\n")))

  (it "should cut off extra items in the row"
    (expect
     (string-table
      :data '(("name" 12 :brown :fox "jumped")
              ("short name" 192 :yellow :what)
              ("abnormally long name" 8 :red)
              ("answer" 42)))
     :to-equal
     (string-join
      '("                name  12"
        "          short name 192"
        "abnormally long name   8"
        "              answer  42")
      "\n")))

  (describe "header"
    (it "should support header"
      (expect
       (string-table
        :header '("who" "mental age" "eye")
        :data '(("name" 12 :brown)
                ("short name" 192 :yellow)
                ("abnormally long name" 8 :red)))
       :to-equal
       (string-join
        '("                 who mental age     eye"
          "                name         12  :brown"
          "          short name        192 :yellow"
          "abnormally long name          8    :red")
        "\n")))

    (it "should support separation"
      (expect
       (string-table
        :header '("who" "mental age" "eye")
        :header-sep "-"
        :header-sep-start "|-"
        :header-sep-conj "-+-"
        :header-sep-end "-|"
        :row-start "| "
        :row-end " |"
        :sep " | "
        :data '(("name" 12 :brown)
                ("short name" 192 :yellow)
                sep
                ("abnormally long name" 8 :red)))
       :to-equal
       (string-join
        '("|                  who | mental age |     eye |"
          "|----------------------+------------+---------|"
          "|                 name |         12 |  :brown |"
          "|           short name |        192 | :yellow |"
          "|----------------------+------------+---------|"
          "| abnormally long name |          8 |    :red |")
        "\n"))))

  (describe "padding"
    (it "should support left padding"
      (expect
       (string-table
        :data '(("name" 12 :brown)
                ("short name" 192 :yellow)
                ("abnormally long name" 8 :red))
        :pad-type 'left)
       :to-equal
       (string-join
        '("                name  12  :brown"
          "          short name 192 :yellow"
          "abnormally long name   8    :red")
        "\n")))

    (it "should support right padding"
      (expect
       (string-table
        :data '(("name" 12 :brown)
                ("short name" 192 :yellow)
                ("abnormally long name" 8 :red))
        :pad-type 'right)
       :to-equal
       (string-join
        '("name                 12  :brown "
          "short name           192 :yellow"
          "abnormally long name 8   :red   ")
        "\n")))

    (it "should support varying padding type"
      (expect
       (string-table
        :data '(("name" 12 :brown)
                ("short name" 192 :yellow)
                ("abnormally long name" 8 :red))
        :pad-type '(right left right))
       :to-equal
       (string-join
        '("name                  12 :brown "
          "short name           192 :yellow"
          "abnormally long name   8 :red   ")
        "\n"))))

  (describe "separation"
    (it "should support custom separator"
      (expect
       (string-table
        :data '(("name" 12 :brown)
                ("short name" 192 :yellow)
                ("abnormally long name" 8 :red))
        :sep " | ")
       :to-equal
       (string-join
        '("                name |  12 |  :brown"
          "          short name | 192 | :yellow"
          "abnormally long name |   8 |    :red")
        "\n"))))

  (describe "column with"
    (it "should support truncation"
      (expect
       (string-table
        :data '(("name" 12 :brown)
                ("short name" 192 :yellow)
                ("abnormally long name" 8 :red))
        :width '(5 full full)
        :sep " | ")
       :to-equal
       (string-join
        '(" name |  12 |  :brown"
          "sh... | 192 | :yellow"
          "ab... |   8 |    :red")
        "\n")))))

(provide 'lib-string-test)
;;; lib-string-test.el ends here
