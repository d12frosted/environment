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

(defun svg-style-from-face (face)
  "Compute the style according to FACE."
  (let* ((font-family (face-attribute face :family nil t))
         (font-weight (face-attribute face :weight nil t))
         (font-size (face-attribute face :height nil t))
         (foreground (face-attribute face :foreground nil t))
         (background (face-attribute face :background nil t)))
    `(:background ,background
      :foreground ,foreground
      :font-family ,font-family
      :font-size ,(if (numberp font-size) (/ font-size 10) font-size)
      :font-weight ,font-weight)))

(defun svg-style-concat (&rest styles)
  "Concatenate STYLES using `svg-style-add'."
  (seq-reduce #'svg-style-add styles nil))

(defun svg-style-add (style-a style-b)
  "Add STYLE-A and STYLE-B.

Operation is right associative."
  (let ((props '(:background
                 :foreground
                 :padding
                 :margin
                 :stroke
                 :radius
                 :alignment
                 :height
                 :scale
                 :font-family
                 :font-size
                 :font-weight)))
    (cl-flet ((plus (p) (or (plist-get style-b p)
                            (plist-get style-a p))))
      (seq-reduce (lambda (l p)
                    (plist-put l p (plus p)))
                  props
                  nil))))

(defvar svg-style-default
  (svg-style-concat
   (svg-style-from-face 'default)
   '(;; In characters (tag and icons) or pixels (progress)
     :padding 0
     ;; In characters
     :margin 0
     ;; In pixels
     :stroke 1
     ;; In pixels
     :radius 3
     ;; Horizontal alignment (in fraction of margin)
     :alignment 0.5
     ;; Ratio of text line height
     :height 0.90
     ;; Icon scaling
     :scale 0.75)))

(defun svg-style-bake (style)
  "Bake STYLE values into SVG supported values."
  (let ((style (seq-copy style)))
    ;; convert Emacs colors to SVG colors
    (when-let ((color (plist-get style :foreground)))
      (plist-put style :foreground (if (equal color 'unspecified)
                                       "none"
                                     (svg-lib-convert-color color))))
    (when-let ((color (plist-get style :background)))
      (plist-put style :background (if (equal color 'unspecified)
                                       "none"
                                     (svg-lib-convert-color color))))

    ;; convert Emacs font weights to SVG font weights
    ;;
    ;; TODO: not sure if the names are right, I've stolen this code
    ;; from svg-lib
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
      (plist-put style :font-weight
                 (or (cdr (assoc (plist-get style :font-weight) weights))
                     (plist-get style :font-weight))))
    style))

(defun svg-icon (collection icon style)
  "Create an SVG image for ICON from COLLECTION using STYLE."
  (let* ((style (svg-style-bake style))
         (root (svg-lib--icon-get-data collection icon))
         (foreground (plist-get style :foreground))
         (background (plist-get style :background))
         (stroke (plist-get style :stroke))
         (height (plist-get style :height))
         (radius (plist-get style :radius))
         (scale (plist-get style :scale))
         (margin (plist-get style :margin))
         (padding (plist-get style :padding))

         (txt-char-width (window-font-width))
         (txt-char-height (window-font-height))

         ;; we say that any icon has width of 2 characters
         (width 2)

         ;; we ignore stroke in height calculations as we want icon to
         ;; be perfectly aligned with text around when inserted
         (box-width (+ (* 2 (or stroke 0))
                       (* (+ width padding) txt-char-width)))
         (box-height (* height txt-char-height))

         (svg-width (+ box-width (* margin txt-char-width)))
         (svg-height box-height)

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar #'string-to-number (split-string viewbox)))
         (icon-x (nth 0 viewbox))
         (icon-y (nth 1 viewbox))
         (icon-width (nth 2 viewbox))
         (icon-height (nth 3 viewbox))
         (scale (* scale (/ (float box-height) (float icon-height))))
         (icon-transform
          (format "translate(%f,%f) scale(%f) translate(%f,%f)"
                  (- icon-x)
                  (- icon-y)
                  scale
                  (- (/ svg-width 2 scale) (/ icon-width 2))
                  (- (/ svg-height 2 scale) (/ icon-height 2))))

         (svg (svg-create svg-width svg-height)))

    (when (and stroke (> stroke 0))
      (svg-rectangle svg stroke stroke
                     (- svg-width (* 2 stroke))
                     (- svg-height (* 2 stroke))
                     :fill (or background "none")
                     :stroke foreground
                     :stroke-width stroke
                     :rx radius))

    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs)))
             ;; (fill (or (cdr (assoc 'fill attrs)) foreground))
             )
        (svg-node svg 'path :d path
                  :fill foreground
                  :transform icon-transform)))
    (svg-lib--image svg :ascent 'center)))

(defun svg-tag (label style)
  "Create an SVG image for LABEL using STYLE."
  (let* ((style (svg-style-bake style))

         (foreground (plist-get style :foreground))
         (background (plist-get style :background))

         (alignment (plist-get style :alignment))
         (stroke (plist-get style :stroke))
         (height (plist-get style :height))
         (radius (plist-get style :radius))
         (padding (plist-get style :padding))
         (font-size (plist-get style :font-size))
         (font-family (plist-get style :font-family))
         (font-weight (plist-get style :font-weight))

         (font-info (font-info (format "%s-%d" font-family font-size)))
         (font-size (aref font-info 2))
         (ascent (aref font-info 8))

         (text-char-width (aref font-info 11))
         (text-char-height (let ((height (aref font-info 3)))
                             (if line-spacing
                                 (+ height line-spacing)
                               height)))

         (tag-width (* (length label) text-char-width))
         (tag-height (* text-char-height height))

         ;; I don't know how to properly calculate that. It seems that
         ;; this value is not linear to length of the text. So doing
         ;; my best guess.
         (spacing-adj (* text-char-width
                         (ceiling (* (sqrt (length label)) 2)
                                  text-char-width)))

         (svg-width (+ (* 2 (or stroke 0))
                       (* 2 padding text-char-width)
                       spacing-adj
                       tag-width))
         (svg-height tag-height)

         (tag-x (+ (* (- svg-width tag-width (* 0.5 spacing-adj)) alignment)
                   (or stroke 0)))
         (tag-y (- ascent (or stroke 0)))

         (svg (svg-create svg-width svg-height)))

    (when (and stroke (> stroke 0))
      (svg-rectangle svg stroke stroke
                     (- svg-width (* 2 stroke))
                     (- svg-height (* 2 stroke))
                     :fill (or background "none")
                     :stroke foreground
                     :stroke-width stroke
                     :rx radius))

    (svg-text svg label
              :font-family font-family
              :font-weight font-weight
              :font-size font-size
              :fill foreground
              :textLength tag-width
              :x tag-x
              :y tag-y)
    (svg-lib--image svg :ascent 'center)))

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
