;;; liquid-types.el --- show inferred liquid-types

;; Copyright (C) 2014 by Ranjit Jhala

;; Author: Ranjit Jhala <jhala@cs.ucsd.edu>
;; Version: 0.0.2
;; Package-Requires: ((flycheck "0.13") (dash "1.2") (emacs "24.1") (popup "0.5.2") (pos-tip "0.5.0") (flycheck-liquidhs "0.0.1") (button-lock "1.0.2"))
;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; see https://github.com/ucsd-progsys/liquidhaskell#emacs

;;; Code:


(eval-when-compile (require 'cl))
(require 'json)
(require 'popup)
(require 'pos-tip nil t)
(require 'thingatpt)
(require 'button-lock)
(require 'flycheck-liquidhs)

;; ------------------------------------------------------------------------
;; Defcustoms
;; ------------------------------------------------------------------------

(defgroup liquid-types-popup nil
  " LiquidHaskell type popup tooltip."
  :group 'haskell
  :prefix "liquid-types/")

(defcustom liquid-types/style 'ascii
  "Set popup style."
  :type '(choice (const :tag "Plain text" ascii)
                 (const :tag "Balloon" balloon))
  :group 'liquid-types)

(defcustom liquid-types/checker-name 'flycheck
  "Checker used by liquid, either 'flycheck or nil.
Prefix for checked files in .liquid"
  :type '(choice (const :tag "Flycheck" flycheck)
                 (const :tag "nil" nil))
  :group 'liquid-types)

(defcustom liquid-types/trigger 'S-double-mouse-1
  "Set trigger event for (liquid-types-show).  Must be a valid Mouse button symbol."
  ;; can either use symbol, choice, radio
  :type '(choice (const :tag "Double click" double-mouse-1)
                 (const :tag "Shift-Double click" S-double-mouse-1)
                 symbol (sexp :tag "Other"))
  :group 'liquid-types)

;; ------------------------------------------------------------------------
;; A structure to represent positions
;; ------------------------------------------------------------------------

(cl-defstruct position file row col)

;; ------------------------------------------------------------------------
;; Utilities for reading json/files
;; ------------------------------------------------------------------------

(defun liquid-types/get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun  liquid-types/get-json-from-file (filePath)
  "Return json object from FILEPATH content."
  (if (file-exists-p filePath)
      (let* ((json-key-type 'string)
             (str ( liquid-types/get-string-from-file filePath)))
        (json-read-from-string str))
    nil))

;; ------------------------------------------------------------------------
;; get/set annot information
;; ------------------------------------------------------------------------

(defun  liquid-types/gethash-nil (key table)
  "Return the value of KEY from TABLE."
  (if table
      (gethash key table nil)
    nil))

(defun  liquid-types/annot-filepath-prefix (mode)
  "Return prefix of annotation file using MODE."
  (if (equal mode 'flycheck)
      "flycheck_"
    nil))

;; (liquid-annot 'flycheck "/path/to/file.hs")
;;    ==> "/path/to/.liquid/flycheck_file.hs.json"
;;
;; (liquid-annot nil       "/path/to/file.hs")
;;    ==> "/path/to/.liquid/file.hs.json"

(defun  liquid-types/annot-filepath (mode file)
  "Return MODE dependent name of annotation FILE."
  (let* ((dir    (file-name-directory file))
         (name   (file-name-nondirectory file))
         (prefix (liquid-types/annot-filepath-prefix mode)))
    (concat dir ".liquid/" prefix name ".json")))

(defvar liquid-types/annot-table (make-hash-table :test 'equal))

;; API
(defun liquid-types/annot-set (file mode)
  "Load information for FILE (in current MODE) into liquid-annot-table."
  (let* ((file-path        (liquid-types/annot-filepath mode file))
         (json-object-type 'hash-table)
         (file-info        (liquid-types/get-json-from-file file-path)))
    (if file-info (puthash file file-info liquid-types/annot-table))))

;; API
(defun liquid-types/annot-get (file row col)
  "Get annotation for identifier in FILE at ROW and COL."
  (let* ((table (liquid-types/gethash-nil file liquid-types/annot-table))
         (r     (format "%d" row))
         (c     (format "%d" col))
         (err   (liquid-types/gethash-nil "errors" table))
         (tys   (liquid-types/gethash-nil "types" table))
         (ro    (liquid-types/gethash-nil r tys)))
    (if tys
        (liquid-types/gethash-nil "ann" (liquid-types/gethash-nil c ro))
      (liquid-types/gethash-nil "message" (elt err 0)))))

;; ------------------------------------------------------------------------
;; Display Annot in Tooltip
;; ------------------------------------------------------------------------

;; For simple, ascii popups, use:
;;    (setq liquid-types-style 'ascii)
;;
;; For emacs', balloon based popups, use:
;;    (setq liquid-types-style 'balloon)

;;(defvar liquid-types-style 'balloon)

(defun liquid-types/pad-line (str)
  "Add extra blanks before and after STR."
  (concat " " str " "))

(defun liquid-types/popup-tip-pad (text)
  "Add extra blanks before and after TEXT."
  (let* ((lines     (split-string text "\n"))
         (pad-lines (mapcar 'liquid-types/pad-line lines))
         (pad-text  (concat "\n" (mapconcat 'identity pad-lines "\n") "\n")))
    (popup-tip pad-text)))

(defun liquid-types/popup-balloon (text)
  "Display TEXT in a balloon popup."
  (liquid-types/popup-tip-pad text))

;; (if (and (functionp 'ac-quick-help-use-pos-tip-p)
;;          (ac-quick-help-use-pos-tip-p))
;;     (pos-tip-show text 'popup-tip-face nil nil 300 popup-tip-max-width)
;;   (popup-tip-pad text)))

(defun liquid-types/popup-ascii (text)
  "Display TEXT in ascii popup."
  (liquid-types/popup-tip-pad text))

(defun liquid-types/popup (text)
  "Display TEXT."
  (let ((f (case liquid-types/style
             ('ascii 'liquid-types/popup-ascii)
             (otherwise 'liquid-types/popup-balloon))))
    (funcall f text)))


;; -- Compute range ---------------------------------------------------------

(defvar liquid-types/id-regexp
      (rx (one-or-more (not (in " \n\t()[]{}")))))

(defvar liquid-types/splitters
      ;; List of  identifier splitters.
      '( ?\s  ?\t ?\n ?\( ?\) ?\[ ?\] ))

(defun liquid-types/is-split (char)
  "Predicate to check if CHAR is a splitter?"
  (member char liquid-types/splitters))

(defun liquid-types/id-start-pos (low p)
  "Find the largest position more than LOW but less than P that is a splitter."
  (let* ((ch (char-before p)))
    (if (or (<= p low) (liquid-types/is-split ch))
        p
      (liquid-types/id-start-pos low (- p 1)))))

(defun liquid-types/column-number-at-pos (pos)
  "Find the column of position POS."
  (+ (- pos (line-beginning-position)) 1))

(defun liquid-types/start-column-number-at-pos (pos)
  "Find the starting column of identifier at POS."
  (let* ((low   (line-beginning-position))
         (start (liquid-types/id-start-pos low pos)))
    (liquid-types/column-number-at-pos start)))

(defsubst liquid-types/get-position ()
  "Return the current cursor position."
  (save-excursion
    (widen)
    (make-position
     :file (expand-file-name (buffer-file-name))
     :row  (line-number-at-pos)
     :col  (liquid-types/start-column-number-at-pos (point)))))

(defun liquid-types/position-string (pos)
  "Return the current POS as a string."
  (format "(%s, %s) in [%s]"
          (position-row pos)
          (position-col pos)
          (position-file pos)))

(defun liquid-types/ident-at-pos (pos)
  "Return the identifier at POS."
  (thing-at-point 'word))

(defun liquid-types/annot-at-pos (pos)
  "Info to display: type annotation for the identifier at POS or NONE."
  (let ((file (position-file pos))
        (row  (position-row  pos))
        (col  (position-col  pos)))
    (liquid-types/annot-get file row col)))

(defun liquid-types-show ()
  "Popup help about anything at point."
  (interactive)
  (let* ((pos    (liquid-types/get-position))
         (ident  (liquid-types/ident-at-pos pos))
         (sorry  (format "No information for %s" ident))
         (annot  (liquid-types/annot-at-pos pos)))
    (if annot
        (liquid-types/popup annot)
      ;; (hdevtools/show-type-info)
      (liquid-types/popup sorry)
      )))

(defun liquid-types/update1 (mode)
  "Update liquid-annot-table by reloading annot file for buffer in MODE."
  (let* ((pos  (liquid-types/get-position))
         (file (position-file pos)))
    (liquid-types/annot-set file mode)))

;;;###autoload
(defun liquid-types-update ()
  (interactive)
  (liquid-types/update1 liquid-types/checker-name))

;; For simple, ascii popups, use:
;;    (liquid-types-init 'ascii)
;; For emacs', balloon based popups, use:
;;    (liquid-types-init 'balloon)
;; or just
;;    (liquid-types-init 'balloon)


;; TODO: this is fugly, refactor it
(defun liquid-types/toggle (&optional args)
  "Initialize/uninitialize liquid-types when the minor mode is toggled."
  (let ((liquid-button-lock-off nil))

    (defun liquid-types-set ()
      ;; turn on button-lock if it isn't already on
      (when (not button-lock-mode)
        (button-lock-mode 1)
        (setq liquid-button-lock-off 1))
      (button-lock-set-button liquid-types/id-regexp
                              'liquid-types-show
                              :mouse-face nil
                              :face nil
                              :face-policy nil
                              :mouse-binding liquid-types/trigger))

    (defun liquid-types-unset ()
      ;; turn off button-lock if we turned it on
      (when liquid-button-lock-off
        (button-lock-mode 0)
        (setq liquid-button-lock-off nil))
      ;;(button-lock-unset-button liquid-button)
      (button-lock-unset-button liquid-types/id-regexp
                                'liquid-types-show
                                :mouse-face nil
                                :face nil
                                :face-policy nil
                                :mouse-binding liquid-types/trigger))

    (if liquid-types-mode
        (liquid-types-set)
      (liquid-types-unset))))


;;;###autoload
(define-minor-mode liquid-types-mode
  "Make the liquid-types tooltip a minor mode."
  nil                                   ; init-value
  " liquidhs-tip"                       ; lighter
  nil                                   ; keymap
  :global nil
  :group 'liquid-types
  (liquid-types/toggle))

(provide 'liquid-types)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; liquid-types.el ends here
