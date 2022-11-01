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
;; Extends `svg' (built-in) and `svg-lib' packages.
;;
;; Nicolas P. Rougier shared many beautiful ideas and packages with
;; Emacs community. One example is `svg-lib' package, which provides
;; several helpers to create SVG images that can be used to improve UX
;; of some buffers.
;;
;; However this library has some issues that were bugging me, so I
;; started to discover ways to overcome them.
;;
;; IMPORTANT! While I criticise `svg-lib', it's a really good package
;; that actually solves one nasty problem brilliantly - proper
;; vertical alignment of icons inside text. Seriously, the main idea
;; here is so simple, but yet it works perfectly well. Of course it
;; has one major limitation - these icons can be part of the file,
;; they are only part of the buffer. So even though it's hard to align
;; icons in `all-the-icons', it still has applications.
;;
;; Here is incomplete list of things that I find troubling or broken.
;; Order is irrelevant.
;;
;; - `svg-lib-concat' is not a closed operation, making it harder to
;;   use it. Official suggestion[1] is to use `svg-lib--image' on
;;   result (and don 't forget to add :ascent).
;;
;; - `svg-lib-concat' messes inner transformations, because it blindly
;;   adds transformation to move second image on canvas. Easy to
;;   notice when you combine multiple `svg-lib-icon's.
;;
;; - In general I find styling confusing in `svg-lib'. For few
;;   reasons.
;;
;;   - It's not well defined. There are basically two functions that
;;     produces a style. First one is `svg-lib-style-compute-default'
;;     that returns a style based on face. And second is
;;     `svg-lib-style' that returns a style based on some other style
;;     and also allows to override some properties. The problem is
;;     that (svg-lib-style (svg-lib-style-compute-default)) is not
;;     equal to (svg-lib-style-compute-default), because
;;     `svg-lib-style` converts some of the values into SVG compatible
;;     format. And this breaks composability.
;;
;;   - Some of the properties should not be part of style. Collection
;;     should be argument to functions that use them, because it's not
;;     a style as collections contain different icons and you always
;;     select an icon from collection.
;;
;; - Inability to defined transparent background. This happens because
;;   stroke is drawn as two rectangles beneath the content. One bigger
;;   that uses foreground colour as fill, and one smaller that uses
;;   background colour as fill. And this setup makes it impossible to
;;   make SVG images with transparent background.
;;
;; - IMO stroke should be created using stroke attribute on rectangle
;;   and not as two rectangles.
;;
;; - In `svg-lib-tag' padding and margin are used incorrectly.
;;   Basically, if we consider stroke as a border, then it should be
;;   like this: margin - border - padding - content.
;;
;; - Calculations are not correct sometimes leading to weird
;;   positioning of content.
;;
;; - Borders are embedded into specific elements instead of being just
;;   a combinator that adds border with some padding. This makes
;;   things a little bit more complex internally in terms of
;;   calculations. But also create the need of hacks like
;;   :crop-{left,right}, which are available only in `svg-lib-tag', so
;;   you can't easily combine an icon and a tag into one bordered
;;   image.
;;
;; [1]: https://github.com/rougier/svg-lib/issues/19
;;
;;; Code:

(require 'dom)
(require 'svg)
(require 'svg-lib)


;; Emacs <-> SVG

(defun svg-convert-color (color)
  "Convert Emacs COLOR to SVG compatible color."
  (if (equal color 'unspecified)
      "none"
    (svg-lib-convert-color color)))

(defun svg-convert-weight (weight)
  "Convert Emacs face WEIGHT to SVG compatible weight."
  (let ((weights
         '((thin       . 100)
           (ultralight . 200)
           (light      . 300)
           (regular    . 400)
           (medium     . 500)
           (semibold   . 600)
           (bold       . 700)
           (extrabold  . 800)
           (black      . 900))))
    (or (cdr (assoc weight weights)) weight)))


;; Widgets

(cl-defun svg-icon (collection icon &key face padding scale)
  "Create an SVG image for ICON from COLLECTION.

FACE is used for styling. When omitted, `default' is used.

PADDING adds extra space before and after the icon.

SCALE is fractional scale of the icon."
  (let* ((face (or face 'default))
         (font-family (face-attribute face :family nil t))
         (font-size (face-attribute face :height nil t))
         (font-size (if (numberp font-size) (/ font-size 10) font-size))
         (foreground (svg-convert-color (face-attribute face :foreground nil t)))
         (font-info (font-info (format "%s-%d" font-family font-size)))

         (root (svg-lib--icon-get-data collection icon))

         (padding (or padding 0))

         (text-char-width (aref font-info 11))

         (text-char-height (let ((height (aref font-info 3)))
                             (if line-spacing
                                 (+ height line-spacing)
                               height)))

         ;; we say that any icon has width of 2 characters
         (label-length 2)

         (box-width (* label-length  text-char-width))
         (box-height text-char-height)

         (svg-width (+ box-width padding padding))
         (svg-height box-height)

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar #'string-to-number (split-string viewbox)))
         (icon-x (nth 0 viewbox))
         (icon-y (nth 1 viewbox))
         (icon-width (nth 2 viewbox))
         (icon-height (nth 3 viewbox))
         (scale (* (or scale 1)
                   (/ (float box-height) (float icon-height))))
         (icon-transform
          (format "translate(%f,%f) scale(%f) translate(%f,%f)"
                  (- icon-x)
                  (- icon-y)
                  scale
                  (- (/ svg-width 2 scale) (/ icon-width 2))
                  (- (/ svg-height 2 scale) (/ icon-height 2))))

         (svg (svg-create svg-width svg-height)))
    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs))))
        (svg-node svg 'path :d path
                  :fill foreground
                  :transform icon-transform)))
    (svg-lib--image svg :ascent 'center)))

(cl-defun svg-label (label &key face padding alignment)
  "Create an SVG image for LABEL.

FACE is used for styling. When omitted, `default' is used.

PADDING adds extra space before and after the label.

ALIGNMENT is horizontal alignment (in fraction of margin). Defaults to 0.5."
  (let* ((face (or face 'default))
         (font-family (face-attribute face :family nil t))
         (font-weight (svg-convert-weight (face-attribute face :weight nil t)))
         (font-size (face-attribute face :height nil t))
         (font-size (if (numberp font-size) (/ font-size 10) font-size))
         (foreground (svg-convert-color (face-attribute face :foreground nil t)))
         (font-info (font-info (format "%s-%d" font-family font-size)))

         (padding (or padding 0))
         (alignment (or alignment 0.5))

         (font-size (aref font-info 2))
         (ascent (aref font-info 8))

         (text-char-width (aref font-info 11))
         (text-char-height (let ((height (aref font-info 3)))
                             (if line-spacing
                                 (+ height line-spacing)
                               height)))

         ;; It seems that text in rendered SVG is a little bit thicker
         ;; than text in Emacs. This means that the text of the
         ;; rendered SVG is longer than regular text. I could not find
         ;; a way to overcome this, so had to adjust for this
         ;; difference.
         ;;
         ;; It brings one important yet obvious conclusion -
         ;; `svg-label' should not be used for regular text.
         (length-adj (* text-char-width
                        (floor (* (sqrt (length label)) 2 1.1)
                               text-char-width)))

         (label-width (+ length-adj (* (length label) text-char-width)))
         (label-height text-char-height)

         (svg-width (+ label-width padding padding))
         (svg-height label-height)

         (label-x (* (- svg-width label-width) alignment))
         (label-y ascent)

         (svg (svg-create svg-width svg-height)))
    (svg-text svg label
              :font-family font-family
              :font-weight font-weight
              :font-size font-size
              :fill foreground
              :x label-x
              :y label-y)
    (svg-lib--image svg :ascent 'center)))


;; Combinators

(cl-defun svg-border (svg-image &key width color radius)
  "Create a border of WIDTH and COLOR around SVG-IMAGE."
  (let* ((svg (car (with-temp-buffer
 	                   (insert (plist-get (cdr svg-image) :data))
 	                   (xml-parse-region (point-min) (point-max)))))
         (attrs (xml-node-attributes svg))
         (svg-width (string-to-number (cdr (assq 'width attrs))))
         (svg-height (string-to-number (cdr (assq 'height attrs))))
         (children (xml-node-children svg))
         (svg (svg-create svg-width svg-height)))
    (svg-rectangle svg width width
                   (- svg-width (* 2 width))
                   (- svg-height (* 2 width))
                   :stroke (svg-convert-color color)
                   :fill "none"
                   :stroke-width width
                   :rx radius)
    (dolist (child children)
      (dom-append-child svg child))
    (svg-lib--image svg :ascent 'center)))

(defun svg-concat (svg-image &rest svg-images)
  "Concatenate SVG-IMAGE and SVG-IMAGES using `svg-append'.

Keep in mind that for practical reasons there is no neutral
element for `svg-append', so you need to provide at least one
image."
  (seq-reduce #'svg-append svg-images svg-image))

(defun svg-append (svg-image-1 svg-image-2)
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
