;;; ghc-dump.el --- Commands for dumping intermediate GHC output.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.10.0") (f "0.17.2") (dash "2.12.1") (magit-popup "20151031.903") (haskell-mode "13.15") (llvm-mode "20150910.644"))

;; This program is free software; you can redistribute it and/or modify
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

;; Utilities for dumping GHC output at various stages, using Stack for stack
;; projects.

;;; Code:

(require 'f nil t)
(require 'dash nil t)
(require 'ghc-core nil t)
(require 'magit-popup nil t)

(autoload 'asm-mode "asm-mode")
(autoload 'llvm-mode "llvm-mode")

(defun ghc-dump--command-with-buffer-setup (buffer-init-fn bufname args &rest dump-flags)
  (save-buffer)
  (let* ((buf (get-buffer-create bufname))
         (neh (lambda () (kill-buffer buf)))
         (ghc-args
          (-flatten (list dump-flags "-c" (buffer-file-name) args))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer))

    (add-hook 'next-error-hook neh)
    (if (ghc-dump--stack-project?)
        (apply #'call-process "stack" nil buf nil "ghc" "--" ghc-args)
      (apply #'call-process "ghc" nil buf nil ghc-args))

    (with-current-buffer buf
      (funcall buffer-init-fn)
      (whitespace-cleanup)
      (ghc-dump-popup-mode +1)
      (read-only-mode +1)
      (goto-char (point-min))
      (display-buffer buf)
      (message "")
      (remove-hook 'next-error-hook neh))))

(defun ghc-dump--stack-project? ()
  (f-traverse-upwards
   (lambda (dir)
     (--any? (s-matches? (rx "stack." (or "yaml" "yml")) it)
             (f-files dir)))))

;;;###autoload
(magit-define-popup ghc-dump-popup
  "Popup console for GHC dump commands."
  'ghc-dump
  :man-page "ghc"
  :switches  '((?o "Optimised" "-O2")
               (?a "All simplifications" "-dsuppress-all")
               (?u "Uniques" "-dsuppress-uniques")
               (?i "Identifier annotations" "-dsuppress-idinfo")
               (?m "Module prefixes" "-dsuppress-module-prefixes")
               (?t "Type signatures" "-dsuppress-type-signatures")
               (?T "Type applications" "-dsuppress-type-applications")
               (?k "Coercions" "-dsuppress-coercions")
               )
  :actions  '((?c "Core" ghc-dump-core)
              (?d "Desugared" ghc-dump-desugared)
              (?C "C--" ghc-dump-opt-cmm)
              (?l "LLVM" ghc-dump-llvm)
              (?a "asm" ghc-dump-asm)
              (?t "Module Types" ghc-dump-types)
              (?p "Template Haskell Splices" ghc-dump-splices)
              (?s "STG" ghc-dump-stg)
              )
  :max-action-columns 3
  :default-action 'ghc-dump-core)

;;;###autoload
(defun ghc-dump-core (&optional args)
  "Dump the GHC Core representation of this buffer after simplification."
  (interactive (list (ghc-dump-arguments)))
  (ghc-dump--command-with-buffer-setup #'ghc-core-mode "*ghc-core*" args "-ddump-simpl"))

;;;###autoload
(defun ghc-dump-desugared (&optional args)
  "Dump the GHC Core representation of the current file."
  (interactive (list (ghc-dump-arguments)))
  (ghc-dump--command-with-buffer-setup #'ghc-core-mode "*ghc-desugared*" args "-ddump-ds"))

;;;###autoload
(defun ghc-dump-opt-cmm (&optional args)
  "Dump the C-- representation of the current file."
  (interactive (list (ghc-dump-arguments)))
  (ghc-dump--command-with-buffer-setup #'ghc-cmm-mode "*ghc-opt-cmm*" args "-ddump-cmm"))

;;;###autoload
(defun ghc-dump-llvm (&optional args)
  "Dump the LLVM representation of the current file."
  (interactive (list (ghc-dump-arguments)))
  (ghc-dump--command-with-buffer-setup #'llvm-mode "*ghc-llvm*" args "-ddump-llvm"))

;;;###autoload
(defun ghc-dump-asm (&optional args)
  "Dump the assembler representation of the current file."
  (interactive (list (ghc-dump-arguments)))
  (ghc-dump--command-with-buffer-setup #'asm-mode "*ghc-asm*" args "-ddump-asm"))

;;;###autoload
(defun ghc-dump-types (&optional args)
  "Dump the types and signatures defined by the current file."
  (interactive (list (ghc-dump-arguments)))
  (ghc-dump--command-with-buffer-setup #'ghc-type-dump-mode "*ghc-types*" args "-ddump-types"))

;;;###autoload
(defun ghc-dump-splices (&optional args)
  "Dump the contents of the current file after Template Haskell expansion."
  (interactive (list (ghc-dump-arguments)))
  (let ((setup (lambda () (ghc-core-mode) (compilation-minor-mode))))
    (ghc-dump--command-with-buffer-setup setup "*ghc-splices*" args "-ddump-splices")))

;;;###autoload
(defun ghc-dump-stg (&optional args)
  "Dump the GHC STG representation of this buffer."
  (interactive (list (ghc-dump-arguments)))
  (ghc-dump--command-with-buffer-setup #'ghc-stg-mode "*ghc-stg*" args "-ddump-stg"))

;;;###autoload
(defvar ghc-dump-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map))

;;;###autoload
(define-minor-mode ghc-dump-popup-mode
  "Minor mode for ghc-dump popup buffers."
  nil nil ghc-dump-popup-mode-map)

;;;###autoload
(define-derived-mode ghc-stg-mode ghc-core-mode "GHC-STG")

;;;###autoload
(define-derived-mode ghc-type-dump-mode haskell-parent-mode "GHC-Types"
  (setq-local font-lock-defaults
              '(haskell-font-lock-choose-keywords
                nil nil ((?\' . "w") (?_  . "w")) nil
                (font-lock-syntactic-keywords
                 . haskell-font-lock-choose-syntactic-keywords)
                (font-lock-syntactic-face-function
                 . haskell-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t))))

(defconst ghc-type-dump-headers
  '("TYPE SIGNATURES"
    "TYPE CONSTRUCTORS"
    "COERCION AXIOMS"
    "INSTANCES"
    "FAMILY INSTANCES"))

(defface ghc-type-dump-section-header
  '((((background light))
     (:bold t :foreground "#2f96dc" :height 1.3 :overline t))
    (((background dark))
     (:bold t :foreground "#2f96dc" :height 1.3 :overline t)))
  "The face used for section headers in a GHC type dump."
  :group 'ghc-dump)

(font-lock-add-keywords
 'ghc-type-dump-mode
 `((,(rx bol (* space) "-- " (* nonl) eol) (0 '(face nil invisible t)))
   (,(concat "^" (regexp-opt ghc-type-dump-headers 'words)) . 'ghc-type-dump-section-header)
   (,(rx bol (* space) "axiom" eow) . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode ghc-cmm-mode c-mode "C--")

(defconst ghc-cmm-keywords
  '("aborts" "align" "aligned" "also" "as" "big" "bits" "byteorder" "case"
    "const," "continuation" "cut" "cuts" "else" "equal" "export" "foreign"
    "goto" "if" "import" "in," "invariant" "invisible" "jump" "little" "memsize"
    "pragma" "reads" "register," "return" "returns" "section" "semi" "span"
    "stackdata" "switch" "target" "targets" "to," "typedef" "unicode" "unwinds"
    "writes"))

(defconst ghc-cmm-types
  '("bits8" "bits16" "bits32" "bits64" "float32" "float64" "I8" "I16" "I32"
    "CInt" "CLong" "I64" "CInt" "CLong" "L_" "F_" "D_"))

(font-lock-add-keywords
 'ghc-cmm-mode
 `((,(regexp-opt ghc-cmm-keywords 'words) . font-lock-keyword-face)
   (,(regexp-opt ghc-cmm-types 'words) . font-lock-type-face)
   (,(rx bol "=====" (* nonl)) . font-lock-comment-face)))

(font-lock-add-keywords 'ghc-core-mode `((,(rx bol "=====" (* nonl)) . font-lock-comment-face)))
(font-lock-add-keywords 'ghc-stg-mode `((,(rx bol "=====" (* nonl)) . font-lock-comment-face)))
(font-lock-add-keywords 'llvm-mode `((,(rx bol "=====" (* nonl)) . font-lock-comment-face)))

(with-eval-after-load 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'asm-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'llvm-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'ghc-core-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'ghc-type-dump-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'ghc-stg-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'ghc-cmm-mode))

(provide 'ghc-dump)

;;; ghc-dump.el ends here
