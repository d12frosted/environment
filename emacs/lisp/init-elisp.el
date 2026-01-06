;;; init-elisp.el --- Main language of the neighborhood -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 08 Feb 2021
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
;; Emacs Lisp enhancements: improved plist/keyword indentation, flycheck
;; integration with elpaca load paths, form-feed display, and buttercup
;; testing support.
;;
;;; Code:

(require 'init-elpa)
(require 'init-ide)



(use-package emacs
  :init
  (setq-default
   ;; Enable indentation+completion using the TAB key.
   ;; `completion-at-point' is often bound to M-TAB.
   tab-always-indent 'complete

   ;; Emacs 30 and newer: Disable Ispell completion function.
   ;; Try `cape-dict' as an alternative.
   text-mode-ispell-word-completion nil

   ;; Hide commands in M-x which do not apply to the current mode.  Corfu
   ;; commands are hidden, since they are not used via M-x. This setting is
   ;; useful beyond Corfu.
   read-extended-command-predicate #'command-completion-default-include-p))

(with-eval-after-load 'corfu
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package flycheck-eldev
  :ensure t
  :after flycheck
  :config
  ;; Setup for Emacs configurations - use regular checker with elpaca load-path
  (defun my-flycheck-setup ()
    "Setup flycheck for Emacs configurations."
    (when-let* ((root (locate-dominating-file (or (buffer-file-name) default-directory) "Eldev")))
      ;; Build load-path with lisp/ and elpaca packages
      (let* ((elpaca-builds-dir (expand-file-name "elpaca/builds"
                                                  (bound-and-true-p path-packages-dir)))
             (elpaca-builds (when (and elpaca-builds-dir
                                       (file-directory-p elpaca-builds-dir))
                              (directory-files elpaca-builds-dir t "\\`[^.]")))
             (load-paths (append (list (expand-file-name "lisp" root))
                                 elpaca-builds)))
        (setq-local flycheck-emacs-lisp-load-path load-paths))
      ;; Disable elisp-eldev since it's not working properly, use regular emacs-lisp
      (setq-local flycheck-disabled-checkers '(elisp-eldev))))

  (add-hook 'emacs-lisp-mode-hook #'my-flycheck-setup 90))

(use-package page-break-lines
  :disabled
  :hook ((emacs-lisp-mode . page-break-lines-mode)))

(use-package form-feed
  :ensure t
  :hook ((emacs-lisp-mode . form-feed-mode)))

(use-package buttercup
  :ensure t
  :defer t
  :config
  (when (version<= "29" emacs-version)
    (defun buttercup-format-spec (format specification)
      "Return a string based on FORMAT and SPECIFICATION.

This is a wrapper around `format-spec', which see. This also adds
a call to `save-match-data', as `format-spec' modifies that."
      (save-match-data
        (format-spec format (--map
                             (cons (car it) (lambda () (cdr it)))
                             specification))))))

(use-package emacsql
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . emacsql-fix-vector-indentation)))



;; default indentation is not perfect when it comes to property lists
;; and face specs; this snipped fixes indent level calculation; I
;; haven't dig too much into this solution, but at least it fixed my
;; problem.
;;
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
(advice-add #'calculate-lisp-indent :override #'elisp-calculate-lisp-indent)
(defun elisp-calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists.

See documentation of `calculate-lisp-indent' for more
information (including meaning of PARSE-START)."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined
  ;; with `defvar` with it's value omitted, marking it special and
  ;; only defining it locally. So if you don't have this, you'll get a
  ;; void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line
                      ;; except the first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      ;; First sexp after `containing-sexp' is a keyword. This
                      ;; condition is more debatable. It's so that I can have
                      ;; unquoted plists in macros. It assumes that you won't
                      ;; make a function whose name is a keyword.
                      ;; (when-let* (char-after (char-after (1+ containing-sexp)))
                      ;;   (char-equal char-after ?:))

                      ;; Check for quotes or backquotes around.
                      (let* ((positions (elt state 9))
                             (last (car (last positions)))
                             (rest (reverse (butlast positions)))
                             (any-quoted-p nil)
                             (point nil))
                        (or
                         (when-let* ((char (char-before last)))
                           (or (char-equal char ?')
                               (char-equal char ?`)))
                         (progn
                           (while (and rest (not any-quoted-p))
                             (setq point (pop rest))
                             (setq any-quoted-p
                                   (or
                                    (when-let* ((char (char-before point)))
                                      (or (char-equal char ?')
                                          (char-equal char ?`)))
                                    (save-excursion
                                      (goto-char (1+ point))
                                      (looking-at-p
                                       "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                           any-quoted-p))))
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(provide 'init-elisp)
;;; init-elisp.el ends here
