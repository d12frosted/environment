;;; barberry-theme.el --- Barberry theme -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 21 May 2022
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
;; Barberry theme is a theme for GNU Emacs heavily inspired by NΛNO
;; Theme design principles. In spite of NΛNO Theme, barberry theme is
;; also based on a small set of colours and faces, uses empty space
;; and font weight to make things beautiful™.
;;
;; The most important difference between these two themes is that
;; barberry theme is designed to be used on laptops and/or in bad
;; lightning. With NΛNO Theme I often found myself not being able to
;; read text either because of font weight being too light or text
;; colour being too subtle.
;;
;; Keep in mind that this is work in progress and it focuses on my
;; personal needs and sense of aesthetic.
;;
;; Barberry theme is designed to be configurable and act as a basis
;; for other applications. Personally I do not pursue a goal to cover
;; all possible modes and packages existing in Emacs ecosystem.
;;
;;; Code:

(defgroup barberry-theme nil
  "Barberry theme."
  :group 'faces)

(defgroup barberry-theme-fonts nil
  "Barberry theme fonts."
  :group 'barberry-theme)

(defgroup barberry-theme-colors nil
  "Barberry theme colors."
  :group 'barberry-theme)



(defcustom barberry-theme-font-mono-family "Source Code Pro"
  "Monospaced font family."
  :group 'barberry-theme-fonts
  :type 'string)

(defcustom barberry-theme-font-mono-size 12
  "Monospaced font size."
  :group 'barberry-theme-fonts
  :type 'natnum)

(defcustom barberry-theme-font-mono-weight 'regular
  "Monospaced font weight."
  :group 'barberry-theme-fonts
  :type 'symbol
  :options '(ultra-condensed
             extra-condensed
             condensed
             semi-condensed
             normal
             regular
             medium
             semi-expanded
             expanded
             extra-expanded
             ultra-expanded))

(defcustom barberry-theme-font-sans-family "Source Sans Pro"
  "Proportional sans font family."
  :group 'barberry-theme-fonts
  :type 'string)

(defcustom barberry-theme-font-sans-size 12
  "Proportional sans font size."
  :group 'barberry-theme-fonts
  :type 'natnum)

(defcustom barberry-theme-font-sans-weight 'regular
  "Proportional sans font weight."
  :group 'barberry-theme-fonts
  :type 'symbol
  :options '(ultra-condensed
             extra-condensed
             condensed
             semi-condensed
             normal
             regular
             medium
             semi-expanded
             expanded
             extra-expanded
             ultra-expanded))

(defcustom barberry-theme-font-serif-family "Source Serif Pro"
  "Proportional sans font family."
  :group 'barberry-theme-fonts
  :type 'string)

(defcustom barberry-theme-font-serif-size 12
  "Proportional serif font size."
  :group 'barberry-theme-fonts
  :type 'natnum)

(defcustom barberry-theme-font-serif-weight 'regular
  "Proportional serif font weight."
  :group 'barberry-theme-fonts
  :type 'symbol
  :options '(ultra-condensed
             extra-condensed
             condensed
             semi-condensed
             normal
             regular
             medium
             semi-expanded
             expanded
             extra-expanded
             ultra-expanded))



(defcustom barberry-theme-color-foreground "#37474F" ;; Blue Grey / L800
  "Colour used as default foreground."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-background "#FFFFFF" ;; White
  "Colour used as default background."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-subtle "#ECEFF1" ;; Blue Grey / L50
  "Color used to suggest a physical area on the screen."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-subtle-i "#FAFAFA" ;; Blue Grey / L50
  "Inverse version of `barberry-theme-color-subtle'."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-faded "#ADADAD"
  "Color used for information of low importance."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-faded-i "#E7ECEE"
  "Inverse version of `barberry-theme-color-faded'."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-salient "#7880B5"
  "Color used for information of big importance."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-salient-i "#E2E5F3"
  "Inverse version of `barberry-theme-color-salient'."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-strong "#263238" ;; Blue Grey / L900
  "Color used for information of structural importance."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-strong-i "#263238"
  "Inverse version of `barberry-theme-color-strong'."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-popout "#FDA521"
  "Color used for information that needs attention."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-popout-i "#FED18C" ;; Deep Champagne
  "Inverse version of `barberry-theme-color-popout'."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-critical "#C8553D"
  "Color used for information that requires immediate action."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-critical-i "#C8553D"
  "Inverse version of `barberry-theme-color-critical'."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-successful "#74AC48"
  "Color used for information that indicates success."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-successful-i "#E9F7DF"
  "Inverse version of `barberry-theme-color-successful'."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-highlight-default "#F5F5F5"
  "Color used to highlight part of the screen."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-highlight-faded "#FAFAFA"
  "Color used to highlight less important part of the screen."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-highlight-successful "#E9F7DF"
  "Color used to highlight successful part of the screen."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-highlight-salient "#F7FCE3"
  "Color used to highlight important part of the screen."
  :group 'barberry-theme-colors
  :type 'color)

(defcustom barberry-theme-color-highlight-critical "#EFC6BE"
  "Color used to highlight critical part of the screen."
  :group 'barberry-theme-colors
  :type 'color)



(defface barberry-theme-face-mono nil
  "Face used for monospaced text.")

(defface barberry-theme-face-sans nil
  "Face used for proportional sans text.")

(defface barberry-theme-face-serif nil
  "Face used for proportional serif text.")

(defface barberry-theme-face-default nil
  "Face used by default.")

(defface barberry-theme-face-default-i nil
  "Inverse version of `barberry-theme-face-default'.")

(defface barberry-theme-face-subtle nil
  "Face used to suggest a physical area on the screen.")

(defface barberry-theme-face-subtle-i nil
  "Inverse version of `barberry-theme-face-subtle'.")

(defface barberry-theme-face-faded nil
  "Face used for information of low importance.")

(defface barberry-theme-face-faded-i nil
  "Inverse version of `barberry-theme-face-faded'.")

(defface barberry-theme-face-salient nil
  "Face used for information of big importance.")

(defface barberry-theme-face-salient-i nil
  "Inverse version of `barberry-theme-face-salient'.")

(defface barberry-theme-face-strong nil
  "Face used for information of structural importance.")

(defface barberry-theme-face-strong-i nil
  "Inverse version of `barberry-theme-face-strong'.")

(defface barberry-theme-face-popout nil
  "Face used for information that needs attention.")

(defface barberry-theme-face-popout-i nil
  "Inverse version of `barberry-theme-face-popout'.")

(defface barberry-theme-face-critical nil
  "Face used for information that requires immediate action.")

(defface barberry-theme-face-critical-i nil
  "Inverse version of `barberry-theme-face-critical'.")

(defface barberry-theme-face-successful nil
  "Face used for information that indicates success.")

(defface barberry-theme-face-successful-i nil
  "Inverse version of `barberry-theme-face-successful'.")



(deftheme barberry
  "Barberry theme. Like a candy.")

(set-face-attribute 'barberry-theme-face-mono nil
                    :family barberry-theme-font-mono-family
                    :height (* 10 barberry-theme-font-mono-size)
                    :weight barberry-theme-font-mono-weight)

(set-face-attribute 'barberry-theme-face-sans nil
                    :family barberry-theme-font-sans-family
                    :height (* 10 barberry-theme-font-sans-size)
                    :weight barberry-theme-font-sans-weight)

(set-face-attribute 'barberry-theme-face-serif nil
                    :family barberry-theme-font-serif-family
                    :height (* 10 barberry-theme-font-serif-size)
                    :weight barberry-theme-font-serif-weight)

(set-face-attribute 'barberry-theme-face-default nil
                    :foreground barberry-theme-color-foreground
                    :inherit 'barberry-theme-face-mono)

(set-face-attribute 'barberry-theme-face-default-i nil
                    :foreground barberry-theme-color-background
                    :background barberry-theme-color-foreground
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-subtle nil
                    :foreground barberry-theme-color-subtle
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-subtle-i nil
                    :background barberry-theme-color-subtle-i
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-faded nil
                    :foreground barberry-theme-color-faded
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-faded-i nil
                    :background barberry-theme-color-faded-i
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-salient nil
                    :foreground barberry-theme-color-salient
                    :weight 'medium
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-salient-i nil
                    :background barberry-theme-color-salient-i
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-strong nil
                    :foreground barberry-theme-color-strong
                    :weight 'medium
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-strong-i nil
                    :foreground barberry-theme-color-background
                    :background barberry-theme-color-strong-i
                    :weight 'medium
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-popout nil
                    :foreground barberry-theme-color-popout
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-popout-i nil
                    :background barberry-theme-color-popout-i
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-critical nil
                    :foreground barberry-theme-color-critical
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-critical-i nil
                    :foreground barberry-theme-color-background
                    :background barberry-theme-color-critical-i
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-successful nil
                    :foreground barberry-theme-color-successful
                    :inherit 'barberry-theme-face-default)

(set-face-attribute 'barberry-theme-face-successful-i nil
                    :foreground barberry-theme-color-foreground
                    :background barberry-theme-color-successful-i
                    :inherit 'barberry-theme-face-default)

(let* ((height (face-attribute 'barberry-theme-face-default :height nil t))
       (height-plus (floor (* 1.2 height))))
  (custom-theme-set-faces
   'barberry
   `(default           ((t (:family ,barberry-theme-font-mono-family
                            :height ,height
                            :weight ,barberry-theme-font-mono-weight
                            :inherit barberry-theme-face-default))))
   '(cursor            ((t (:inherit barberry-theme-face-default-i))))
   `(highlight         ((t (:background ,barberry-theme-color-highlight-default))))
   '(bold              ((t (:inherit barberry-theme-face-strong))))
   '(italic            ((t (:inherit barberry-theme-face-faded))))
   '(bold-italic       ((t (:inherit barberry-theme-face-strong))))
   `(region            ((t (:background ,barberry-theme-color-highlight-salient))))
   '(fringe            ((t (:inherit (barberry-theme-face-faded)))))
   '(hl-line           ((t (:inherit highlight))))
   '(link              ((t (:inherit barberry-theme-face-salient))))
   '(fixed-pitch       ((t (:inherit default))))
   '(fixed-pitch-serif ((t (:inherit default))))
   '(variable-pitch    ((t (:inherit barberry-theme-face-sans))))
   '(shadow            ((t (:inherit barberry-theme-face-faded))))
   '(success           ((t (:inherit barberry-theme-face-successful))))
   '(warning           ((t (:inherit barberry-theme-face-popout))))
   '(error             ((t (:inherit barberry-theme-face-critical))))
   '(match             ((t (:inherit barberry-theme-face-popout))))

   ;; mode-line
   `(mode-line           ((t (:foreground ,barberry-theme-color-background
                              :background ,barberry-theme-color-foreground
                              :box (:line-width 3
                                    :color ,barberry-theme-color-foreground
                                    :style nil)))))
   `(mode-line-highlight ((t (:inherit barberry-theme-face-popout))))
   `(mode-line-buffer-id ((t (:weight medium))))
   `(mode-line-emphasis  ((t (:weight medium))))
   `(mode-line-inactive  ((t (:foreground ,barberry-theme-color-background
                              :background ,barberry-theme-color-faded
                              :box (:line-width 3
                                    :color ,barberry-theme-color-faded
                                    :style nil)))))

   ;; window divider
   `(window-divider             ((t (:foreground ,barberry-theme-color-background))))
   '(window-divider-first-pixel ((t (:inherit window-divider))))
   '(window-divider-last-pixel  ((t (:inherit window-divider))))
   `(vertical-border            ((t (:foreground ,barberry-theme-color-background))))

   ;; general
   '(buffer-menu-buffer           ((t (:inherit barberry-theme-face-strong))))
   '(minibuffer-prompt            ((t (:inherit barberry-theme-face-strong))))
   '(isearch                      ((t (:inherit barberry-theme-face-popout-i))))
   '(isearch-fail                 ((t (:inherit barberry-theme-face-faded))))
   '(show-paren-match             ((t (:inherit barberry-theme-face-strong))))
   '(show-paren-mismatch          ((t (:inherit barberry-theme-face-critical))))
   '(lazy-highlight               ((t (:inherit barberry-theme-face-subtle))))
   '(trailing-whitespace          ((t (:inherit barberry-theme-face-subtle))))
   '(secondary-selection          ((t (:inherit barberry-theme-face-subtle))))
   '(completions-annotations      ((t (:inherit barberry-theme-face-faded))))
   '(completions-common-part      ((t (:inherit barberry-theme-face-strong))))
   '(completions-first-difference ((t (:inherit barberry-theme-face-default))))
   '(tooltip                      ((t (:inherit barberry-theme-face-subtle))))
   '(read-multiple-choice-face    ((t (:inherit barberry-theme-face-strong))))
   '(nobreak-hyphen               ((t (:inherit barberry-theme-face-popout))))
   '(nobreak-space                ((t (:inherit barberry-theme-face-popout))))
   '(help-argument-name           ((t (:inherit barberry-theme-face-faded))))
   `(help-key-binding             ((t (:foreground ,barberry-theme-color-foreground
                                       :background ,barberry-theme-color-highlight-salient
                                       :inherit fixed-pitch))))
   `(header-line                  ((t (:foreground ,barberry-theme-color-foreground
                                       :background ,barberry-theme-color-highlight-default
                                       :box nil
                                       :inherit mode-line))))
   '(tabulated-list-fake-header   ((t (:inherit barberry-theme-face-strong))))
   '(tool-bar                     ((t (:inherit barberry-theme-face-faded-i))))

   ;; font lock
   '(font-lock-comment-face       ((t (:inherit barberry-theme-face-faded))))
   '(font-lock-doc-face           ((t (:inherit barberry-theme-face-faded))))
   '(font-lock-string-face        ((t (:inherit barberry-theme-face-default))))
   '(font-lock-constant-face      ((t (:inherit barberry-theme-face-salient))))
   '(font-lock-warning-face       ((t (:inherit barberry-theme-face-popout))))
   '(font-lock-function-name-face ((t (:inherit barberry-theme-face-salient))))
   '(font-lock-variable-name-face ((t (:inherit barberry-theme-face-salient))))
   '(font-lock-builtin-face       ((t (:inherit barberry-theme-face-salient))))
   '(font-lock-type-face          ((t (:inherit barberry-theme-face-salient))))
   '(font-lock-keyword-face       ((t (:inherit barberry-theme-face-salient))))

   ;; custom edit
   '(widget-field             ((t (:inherit barberry-theme-face-faded-i))))
   '(widget-button            ((t (:inherit barberry-theme-face-strong))))
   '(widget-inactive          ((t (:inherit shadow))))
   '(widget-single-line-field ((t (:inherit barberry-theme-face-subtle))))
   '(custom-group-subtitle    ((t (:inherit barberry-theme-face-strong))))
   '(custom-group-tag         ((t (:inherit barberry-theme-face-strong))))
   '(custom-group-tag-1       ((t (:inherit barberry-theme-face-strong))))
   '(custom-comment           ((t (:inherit barberry-theme-face-faded))))
   '(custom-comment-tag       ((t (:inherit barberry-theme-face-faded))))
   '(custom-changed           ((t (:inherit barberry-theme-face-salient))))
   '(custom-modified          ((t (:inherit barberry-theme-face-salient))))
   '(custom-face-tag          ((t (:inherit barberry-theme-face-strong))))
   '(custom-variable-tag      ((t (:inherit barberry-theme-face-strong))))
   '(custom-invalid           ((t (:inherit barberry-theme-face-popout))))
   '(custom-visibility        ((t (:inherit barberry-theme-face-salient))))
   '(custom-state             ((t (:inherit barberry-theme-face-salient))))
   '(custom-link              ((t (:inherit barberry-theme-face-salient))))
   '(custom-variable-obsolete ((t (:inherit barberry-theme-face-faded))))
   `(custom-button            ((t (:weight medium
                                   :foreground ,barberry-theme-color-salient
                                   :box (:line-width (2 . 2)
                                         :color ,barberry-theme-color-subtle
                                         :style flat)
                                   :inherit barberry-theme-face-default))))

   ;; company
   '(company-tooltip                      ((t (:inherit barberry-theme-face-faded))))
   '(company-tooltip-mouse                ((t (:inherit barberry-theme-face-faded-i))))
   `(company-tooltip-selection            ((t (:background ,barberry-theme-color-highlight-default))))

   '(company-scrollbar-fg                 ((t (:inherit barberry-theme-face-default-i))))
   '(company-scrollbar-bg                 ((t (:inherit barberry-theme-face-faded-i))))

   '(company-tooltip-scrollbar-thumb      ((t (:inherit barberry-theme-face-default-i))))
   '(company-tooltip-scrollbar-track      ((t (:inherit barberry-theme-face-faded-i))))

   '(company-tooltip-common               ((t (:inherit barberry-theme-face-strong))))
   '(company-tooltip-common-selection     ((t (:inherit barberry-theme-face-salient-i))))
   '(company-tooltip-annotation           ((t (:inherit barberry-theme-face-faded))))
   '(company-tooltip-annotation-selection ((t (:inherit barberry-theme-face-faded))))

   ;; info
   '(info-node        ((t (:inherit barberry-theme-face-strong))))
   '(info-menu-header ((t (:inherit barberry-theme-face-strong))))
   '(info-header-node ((t (:inherit barberry-theme-face-default))))
   '(info-index-match ((t (:inherit barberry-theme-face-salient))))
   `(info-title-1     ((t (:overline ,barberry-theme-color-subtle
                           :height ,height-plus
                           :inherit barberry-theme-face-strong))))
   `(info-title-2     ((t (:overline ,barberry-theme-color-subtle
                           :inherit barberry-theme-face-strong))))
   '(info-title-3     ((t (:inherit barberry-theme-face-strong))))
   '(info-title-4     ((t (:inherit barberry-theme-face-strong))))
   '(Info-quoted      ((t (:inherit barberry-theme-face-faded))))
   '(info-menu-star   ((t (:inherit barberry-theme-face-critical))))

   ;; outline
   `(outline-1 ((t (:overline ,barberry-theme-color-subtle
                    :height ,height-plus
                    :inherit barberry-theme-face-strong))))
   `(outline-2 ((t (:overline ,barberry-theme-color-subtle
                    :inherit barberry-theme-face-strong))))
   '(outline-3 ((t (:inherit barberry-theme-face-strong))))
   '(outline-4 ((t (:inherit barberry-theme-face-strong))))
   '(outline-5 ((t (:inherit barberry-theme-face-strong))))
   '(outline-6 ((t (:inherit barberry-theme-face-strong))))
   '(outline-7 ((t (:inherit barberry-theme-face-strong))))
   '(outline-8 ((t (:inherit barberry-theme-face-strong))))

   ;; org-agenda
   '(org-agenda-calendar-event   ((t (:inherit barberry-theme-face-default))))
   '(org-agenda-calendar-sexp    ((t (:inherit barberry-theme-face-salient))))
   '(org-agenda-clocking         ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-column-dateline  ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-current-time     ((t (:inherit barberry-theme-face-strong))))
   '(org-agenda-date             ((t (:inherit barberry-theme-face-salient))))
   '(org-agenda-date-today       ((t (:inherit (barberry-theme-face-salient
                                                barberry-theme-face-strong)))))
   '(org-agenda-date-weekend     ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-diary            ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-dimmed-todo-face ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-done             ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-filter-category  ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-filter-effort    ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-filter-regexp    ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-filter-tags      ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-property-face    ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-restriction-lock ((t (:inherit barberry-theme-face-faded))))
   '(org-agenda-structure        ((t (:inherit barberry-theme-face-strong))))

   ;; org
   '(org-archived                 ((t (:inherit barberry-theme-face-faded))))
   '(org-block                    ((t (:inherit highlight))))
   `(org-block-begin-line         ((t (:inherit barberry-theme-face-faded
                                       :underline ,(face-background
                                                    'barberry-theme-face-subtle)))))
   `(org-block-end-line           ((t (:inherit barberry-theme-face-faded
                                       :overline ,(face-background
                                                   'barberry-theme-face-subtle)))))
   '(org-checkbox                 ((t (:inherit barberry-theme-face-faded))))
   '(org-checkbox-statistics-done ((t (:inherit barberry-theme-face-faded))))
   '(org-checkbox-statistics-todo ((t (:inherit barberry-theme-face-faded))))
   '(org-clock-overlay            ((t (:inherit barberry-theme-face-faded))))
   '(org-code                     ((t (:inherit barberry-theme-face-salient))))
   '(org-column                   ((t (:inherit barberry-theme-face-faded))))
   '(org-column-title             ((t (:inherit barberry-theme-face-faded))))
   '(org-date                     ((t (:inherit barberry-theme-face-faded))))
   '(org-date-selected            ((t (:inherit barberry-theme-face-popout-i))))
   '(org-default                  ((t (:inherit barberry-theme-face-faded))))
   '(org-document-info            ((t (:inherit barberry-theme-face-faded))))
   '(org-document-info-keyword    ((t (:inherit barberry-theme-face-faded))))
   `(org-document-title           ((t (:height ,height-plus
                                       :inherit barberry-theme-face-strong))))
   '(org-done                     ((t (:inherit barberry-theme-face-faded))))
   '(org-drawer                   ((t (:inherit barberry-theme-face-faded))))
   '(org-ellipsis                 ((t (:inherit barberry-theme-face-faded))))
   '(org-footnote                 ((t (:inherit barberry-theme-face-faded))))
   '(org-formula                  ((t (:inherit barberry-theme-face-faded))))
   '(org-headline-done            ((t (:inherit barberry-theme-face-faded))))
   ;; '(org-hide                  ((t (:inherit barberry-theme-face-subtle-i))))
   ;; '(org-indent                ((t (:inherit barberry-theme-face-subtle-i))))
   '(org-latex-and-related        ((t (:inherit barberry-theme-face-faded))))
   `(org-level-1                  ((t (:overline ,barberry-theme-color-subtle
                                       :height ,height-plus
                                       :inherit barberry-theme-face-strong))))
   `(org-level-2                  ((t (:overline ,barberry-theme-color-subtle
                                       :inherit barberry-theme-face-strong))))
   '(org-level-3                  ((t (:inherit barberry-theme-face-strong))))
   '(org-level-4                  ((t (:inherit barberry-theme-face-strong))))
   '(org-level-5                  ((t (:inherit barberry-theme-face-strong))))
   '(org-level-6                  ((t (:inherit barberry-theme-face-strong))))
   '(org-level-7                  ((t (:inherit barberry-theme-face-strong))))
   '(org-level-8                  ((t (:inherit barberry-theme-face-strong))))
   '(org-link                     ((t (:inherit barberry-theme-face-salient))))
   '(org-list-dt                  ((t (:inherit barberry-theme-face-faded))))
   '(org-macro                    ((t (:inherit barberry-theme-face-faded))))
   '(org-meta-line                ((t (:inherit barberry-theme-face-faded))))
   '(org-mode-line-clock          ((t (:inherit barberry-theme-face-faded))))
   '(org-mode-line-clock-overrun  ((t (:inherit barberry-theme-face-faded))))
   '(org-priority                 ((t (:inherit barberry-theme-face-faded))))
   '(org-property-value           ((t (:inherit barberry-theme-face-faded))))
   '(org-quote                    ((t (:inherit barberry-theme-face-faded))))
   '(org-scheduled                ((t (:inherit barberry-theme-face-faded))))
   '(org-scheduled-previously     ((t (:inherit barberry-theme-face-faded))))
   '(org-scheduled-today          ((t (:inherit barberry-theme-face-faded))))
   '(org-sexp-date                ((t (:inherit barberry-theme-face-faded))))
   '(org-special-keyword          ((t (:inherit barberry-theme-face-faded))))
   '(org-table                    ((t (:inherit barberry-theme-face-faded))))
   '(org-tag                      ((t (:inherit barberry-theme-face-popout))))
   '(org-tag-group                ((t (:inherit barberry-theme-face-faded))))
   '(org-target                   ((t (:inherit barberry-theme-face-faded))))
   '(org-time-grid                ((t (:inherit barberry-theme-face-faded))))
   '(org-todo                     ((t (:weight medium
                                       :inherit barberry-theme-face-critical))))
   '(org-upcoming-deadline        ((t (:inherit barberry-theme-face-popout))))
   `(org-verbatim                 ((t (:background ,barberry-theme-color-highlight-default
                                       :inherit barberry-theme-face-default))))
   '(org-verse                    ((t (:inherit barberry-theme-face-faded))))
   '(org-warning                  ((t (:inherit barberry-theme-face-popout))))

   ;; markdown
   '(markdown-blockquote-face         ((t (:inherit barberry-theme-face-default))))
   '(markdown-bold-face               ((t (:inherit barberry-theme-face-strong))))
   '(markdown-code-face               ((t (:extend t :inherit highlight))))
   '(markdown-comment-face            ((t (:inherit barberry-theme-face-faded))))
   '(markdown-footnote-marker-face    ((t (:inherit barberry-theme-face-default))))
   '(markdown-footnote-text-face      ((t (:inherit barberry-theme-face-default))))
   '(markdown-gfm-checkbox-face       ((t (:inherit barberry-theme-face-default))))
   '(markdown-header-delimiter-face   ((t (:inherit barberry-theme-face-faded))))
   '(markdown-header-face             ((t (:inherit barberry-theme-face-strong))))
   `(markdown-header-face-1           ((t (:overline ,barberry-theme-color-subtle
                                           :height ,height-plus
                                           :inherit barberry-theme-face-strong))))
   `(markdown-header-face-2           ((t (:overline ,barberry-theme-color-subtle
                                           :inherit barberry-theme-face-strong))))
   '(markdown-header-face-3           ((t (:inherit barberry-theme-face-strong))))
   '(markdown-header-face-4           ((t (:inherit barberry-theme-face-strong))))
   '(markdown-header-face-5           ((t (:inherit barberry-theme-face-strong))))
   '(markdown-header-face-6           ((t (:inherit barberry-theme-face-strong))))
   '(markdown-header-rule-face        ((t (:inherit barberry-theme-face-default))))
   '(markdown-highlight-face          ((t (:inherit barberry-theme-face-default))))
   '(markdown-hr-face                 ((t (:inherit barberry-theme-face-default))))
   '(markdown-html-attr-name-face     ((t (:inherit barberry-theme-face-default))))
   '(markdown-html-attr-value-face    ((t (:inherit barberry-theme-face-default))))
   '(markdown-html-entity-face        ((t (:inherit barberry-theme-face-default))))
   '(markdown-html-tag-delimiter-face ((t (:inherit barberry-theme-face-default))))
   '(markdown-html-tag-name-face      ((t (:inherit barberry-theme-face-default))))
   '(markdown-inline-code-face        ((t (:weight medium
                                           :inherit barberry-theme-face-subtle-i))))
   '(markdown-italic-face             ((t (:inherit barberry-theme-face-faded))))
   '(markdown-language-info-face      ((t (:inherit barberry-theme-face-default))))
   '(markdown-language-keyword-face   ((t (:inherit barberry-theme-face-faded))))
   '(markdown-line-break-face         ((t (:inherit barberry-theme-face-default))))
   '(markdown-link-face               ((t (:inherit barberry-theme-face-salient))))
   '(markdown-link-title-face         ((t (:inherit barberry-theme-face-default))))
   '(markdown-list-face               ((t (:inherit barberry-theme-face-faded))))
   '(markdown-markup-face             ((t (:inherit barberry-theme-face-faded))))
   '(markdown-math-face               ((t (:inherit barberry-theme-face-default))))
   '(markdown-metadata-key-face       ((t (:inherit barberry-theme-face-faded))))
   '(markdown-metadata-value-face     ((t (:inherit barberry-theme-face-faded))))
   '(markdown-missing-link-face       ((t (:inherit barberry-theme-face-default))))
   '(markdown-plain-url-face          ((t (:inherit barberry-theme-face-default))))
   `(markdown-pre-face                ((t (:extend t
                                           :background ,barberry-theme-color-highlight-faded
                                           :inherit highlight))))
   '(markdown-reference-face          ((t (:inherit barberry-theme-face-faded))))
   '(markdown-strike-through-face     ((t (:inherit barberry-theme-face-faded))))
   '(markdown-table-face              ((t (:inherit barberry-theme-face-default))))
   '(markdown-url-face                ((t (:inherit barberry-theme-face-salient))))

   ;; popup
   '(popup-face                       ((t (:inherit highlight))))
   '(popup-isearch-match              ((t (:inherit barberry-theme-face-popout))))
   '(popup-menu-face                  ((t (:inherit barberry-theme-face-subtle))))
   '(popup-menu-mouse-face            ((t (:inherit barberry-theme-face-faded-i))))
   '(popup-menu-selection-face        ((t (:inherit barberry-theme-face-salient-i))))
   '(popup-menu-summary-face          ((t (:inherit barberry-theme-face-faded))))
   '(popup-scroll-bar-background-face ((t (:inherit barberry-theme-face-subtle))))
   '(popup-scroll-bar-foreground-face ((t (:inherit barberry-theme-face-subtle))))
   '(popup-summary-face               ((t (:inherit barberry-theme-face-faded))))
   '(popup-tip-face                   ((t (:inherit barberry-theme-face-popout-i))))

   ;; diff-hl-mode
   `(diff-hl-change ((t (:background ,barberry-theme-color-highlight-salient))))
   `(diff-hl-insert ((t (:background ,barberry-theme-color-highlight-successful))))
   `(diff-hl-delete ((t (:background ,barberry-theme-color-highlight-critical))))

   ;; lsp-ui
   `(lsp-ui-doc-background ((t (:background ,barberry-theme-color-background))))

   ;; child frame (used by ls-ui-doc)
   `(child-frame-border ((t (:background ,barberry-theme-color-subtle))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:style wave :color ,barberry-theme-color-salient)))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,barberry-theme-color-critical)))))

   ;; page-break
   '(page-break-lines ((t (:inherit barberry-theme-face-subtle))))
   `(form-feed-line ((t (:strike-through ,barberry-theme-color-subtle))))

   `(restclient-header-value-face ((t (:inherit font-lock-string-face))))))

(with-eval-after-load 'lsp-ui-doc
  ;; ideally it should use `child-frame-border', but it does not
  (setq-default lsp-ui-doc-border barberry-theme-color-subtle))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; Add theme folder to `custom-theme-load-path' when loading file.
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))



(provide 'barberry-theme)
;;; barberry-theme.el ends here
