;;; lang/scala/autoload/lsp-scala.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Ross A. Baker <ross@rossabaker.com>
;;         Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 27 Dec 2018
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun +scala-comment-indent-new-line (&rest _)
  "Continue the commnt on the current line.

Meant to be used for `scala-mode's
`comment-line-break-function'."
  (let* ((state (syntax-ppss))
         (comment-start-pos (nth 8 state)))
    (save-match-data
      (cond ((and (integerp (nth 4 state))
                  ;; Ensure that we're inside a scaladoc comment
                  (string-match-p "^/\\*\\*?[^\\*]?"
                                  (buffer-substring-no-properties
                                   comment-start-pos
                                   (min (+ comment-start-pos 4)
                                        (point-max))))
                  (progn
                    (setq prev-line (buffer-substring-no-properties
                                     (line-beginning-position 0)
                                     (line-end-position 0)))
                    (or (string-match "^\\s-*\\*" prev-line)
                        (string-match "\\s-*/*" prev-line))))
             (newline nil t)
             (indent-according-to-mode)
             (insert (make-string (max 0 (- (1- (match-end 0))
                                            (match-beginning 0)))
                                  ? )
                     "*")
             (scala-indent:indent-on-scaladoc-asterisk))
            ((nth 4 state) ; for line comments
             (call-interactively #'comment-indent-new-line))
            (t
             (newline nil t)
             (indent-according-to-mode))))))
