;;; lib-svg.el --- SVG libary   -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 22 May 2022
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
;; Extends `svg' and `lib-svg'.
;;
;;; Code:

(defun svg-concat (svg-image-1 svg-image-2)
  "Concatenate SVG-IMAGE-1 and SVG-IMAGE-2 images horizontally.

Resulting type is the same as its arguments.

There are two ways to concatenate two images:

- using groups (e.g. g element);
- nesting svg elements (e.g. creating parent svg with two children).

I am not sure which approach is better, but I definitely don't
like the fact that groups can't have position, you can only
transform them.

This function takes roots in `svg-lib-concat', but avoids messing
up with transformations in its arguments. E.g. if you have image
with scaling, then concatenating it twice results in images of
different size. This function avoids that. In addition, it is
consistent with it's input type and kind of forms monoid."
  (let* ((svg-1 (car (with-temp-buffer
 	                     (insert (plist-get (cdr svg-image-1) :data))
 	                     (xml-parse-region (point-min) (point-max)))))
         (attrs (xml-node-attributes svg-1))
         (width-1 (string-to-number (cdr (assq 'width attrs))))
         (height-1 (string-to-number (cdr (assq 'height attrs))))

         (svg-2 (car (with-temp-buffer
 	                     (insert (plist-get (cdr svg-image-2) :data))
 	                     (xml-parse-region (point-min) (point-max)))))
         (attrs (xml-node-attributes svg-2))
         (width-2 (string-to-number (cdr (assq 'width attrs))))
         (height-2 (string-to-number (cdr (assq 'height attrs))))

         (width (+ width-1 width-2))
         (height (max height-1 height-2))
         (svg (svg-create width height)))
    (dom-append-child svg svg-1)
    (dom-set-attribute svg-2 'x width-1)
    (dom-append-child svg svg-2)
    (svg-lib--image svg :ascent 'center)))

(provide 'lib-svg)
;;; lib-svg.el ends here
