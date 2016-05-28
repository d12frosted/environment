;;; d12-ukrainian-input-method.el --- custom input method

;; Copyright (c) 2016 Boris Buliga

;; Author: Boris Buliga <d12frosted@gmail.com>
;; Maintainer: Boris Buliga <d12frosted@gmail.com>
;; Created: 26 May 2016

;; Keywords:
;; Homepage:

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(quail-define-package
 "d12-ukrainian" "UTF-8" "UK" t "Input method for Ukrainian based
 on OS X Ukrainian keyboard layout with some custom
 modifications." nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("`" ?`)
 ("1" ?1)
 ("2" ?2)
 ("3" ?3)
 ("4" ?4)
 ("5" ?5)
 ("6" ?6)
 ("7" ?7)
 ("8" ?8)
 ("9" ?9)
 ("0" ?0)
 ("-" ?-)
 ("=" ?=)
 ("q" ?й)
 ("w" ?ц)
 ("e" ?у)
 ("r" ?к)
 ("t" ?е)
 ("y" ?н)
 ("u" ?г)
 ("i" ?ш)
 ("o" ?щ)
 ("p" ?з)
 ("[" ?х)
 ("]" ?ї)
 ("\\" ?ґ)
 ("a" ?ф)
 ("s" ?и)
 ("d" ?в)
 ("f" ?а)
 ("g" ?п)
 ("h" ?р)
 ("j" ?о)
 ("k" ?л)
 ("l" ?д)
 (";" ?ж)
 ("'" ?є)
 ("z" ?я)
 ("x" ?ч)
 ("c" ?с)
 ("v" ?м)
 ("b" ?і)
 ("n" ?т)
 ("m" ?ь)
 ("," ?б)
 ("." ?ю)
 ("/" ?/)

 ("~" ?~)
 ("!" ?!)
 ("@" ?\")
 ("#" ?#)
 ("$" ?\%)
 ("%" ?:)
 ("^" ?,)
 ("&" ?\.)
 ("*" ?\;)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?\+)
 ("Q" ?Й)
 ("W" ?Ц)
 ("E" ?У)
 ("R" ?К)
 ("T" ?Е)
 ("Y" ?Н)
 ("U" ?Г)
 ("I" ?Ш)
 ("O" ?Щ)
 ("P" ?З)
 ("{" ?Х)
 ("}" ?Ї)
 ("|" ?Ґ)
 ("A" ?Ф)
 ("S" ?И)
 ("D" ?В)
 ("F" ?А)
 ("G" ?П)
 ("H" ?Р)
 ("J" ?О)
 ("K" ?Л)
 ("L" ?Д)
 (":" ?Ж)
 ("\"" ?Є)
 ("Z" ?Я)
 ("X" ?Ч)
 ("C" ?С)
 ("V" ?М)
 ("B" ?І)
 ("N" ?Т)
 ("M" ?Ь)
 ("<" ?Б)
 (">" ?Ю)
 ("?" ?\?)

 ("¢" ?$)

 ("ß" ?ы)
 ("‘" ?ъ)
 ("æ" ?э)
 ("Æ" ?Э)
 ("«" ?ё)
 ("’" ?Ъ)
 ("»" ?Ё))

(provide 'd12-ukrainian-input-method)
;;; d12-ukrainian-input-method.el ends here
