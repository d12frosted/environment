;;; lisp-funcs.el --- funcs file of lisp configurations
;;
;; Copyright (c) 2014-2015 Boris Buliga
;;
;; Author: Boris Buliga <d12frosted@icloud.com>
;; Maintainer: Boris Buliga <d12frosted@icloud.com>
;; Created: 11 May 2015
;;
;; URL: https://github.com/d12frosted/environment
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Code

(defun d12/eval-current-form ()
  "Looks for the current def* or set* command then evaluates, unlike `eval-defun', does not go to topmost function"
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun d12/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (eval-last-sexp prefix)))

;; thanks to Chris Done
(defun d12/paredit-kill-sexp ()
  "Kill the sexp at point."
  (interactive)
  (if (eq last-command 'kill-region)
      (call-interactively 'kill-sexp)
    (cond
     ((paredit-in-string-p)
      (paredit-backward-up)
      (call-interactively 'kill-sexp))
     ((paredit-inside-sexp-p)
      (paredit-backward)
      (call-interactively 'kill-sexp))
     ((paredit-start-of-sexp-p)
      (call-interactively 'kill-sexp))
     (t
      (paredit-backward)
      (call-interactively 'kill-sexp)))))

(defun paredit-delete-sexp ()
  "Delete the sexp at point."
  (interactive)
  (cond
   ((paredit-in-comment-p)
    (call-interactively 'delete-char))
   ;; Strings don't behave the same as normal sexps in paredit.
   ((paredit-in-string-p)
    (delete-region (save-excursion (paredit-backward-up)
                                   (point))
                   (save-excursion (paredit-backward-up)
                                   (paredit-forward)
                                   (point))))
   ((paredit-inside-sexp-p)
    (delete-region (save-excursion (paredit-backward)
                                   (point))
                   (save-excursion (paredit-forward)
                                   (point))))
   ((paredit-start-of-sexp-p)
    (delete-region (point)
                   (save-excursion (paredit-forward)
                                   (point))))
   ;; Otherwise we're at the end of a sexp.
   (t
    (delete-region (save-excursion (paredit-backward)
                                   (point))
                   (save-excursion (paredit-backward)
                                   (paredit-forward)
                                   (point))))))

(defun paredit-inside-sexp-p ()
  "Are we inside the bounds of a sexp?"
  (= (save-excursion (paredit-forward)
                     (point))
     (save-excursion (paredit-backward)
                     (paredit-forward)
                     (point))))

(defun paredit-start-of-sexp-p ()
  "Are we at the start of a sexp?"
  (= (save-excursion (paredit-forward)
                     (paredit-backward)
                     (point))
     (point)))
