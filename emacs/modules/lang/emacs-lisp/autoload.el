;;; lang/emacs-lisp/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2018
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
(defvar +emacs-lisp-flycheck-iniquity-files nil
  "List of filenames that should not be flychecked.")

;;;###autoload
(defun +emacs-lisp|disable-flycheck-maybe ()
  "Disable flycheck-mode if in emacs.d."
  (when (and (bound-and-true-p flycheck-mode)
             (eq major-mode 'emacs-lisp-mode)
             (or (not buffer-file-name)
                 (cl-loop for dir in (list nucleus-emacs-dir)
                          if (file-in-directory-p buffer-file-name dir)
                          return t)
                 (seq-contains +emacs-lisp-flycheck-iniquity-files
                               (file-name-nondirectory buffer-file-name))))
    (flycheck-mode -1)))

;;;###autoload
(defun +emacs-lisp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line
long), otherwise to a pop up buffer."
  (require 'pp)
  (let ((result
         (let ((debug-on-error t))
           (eval (read
                  (concat "(progn "
                          (buffer-substring-no-properties beg end)
                          "\n)"))
                 t)))
        (buf (get-buffer-create "*eval*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (read-only-mode +1)
      (erase-buffer)
      (setq-local scroll-margin 0)
      (let (emacs-lisp-mode-hook)
        (emacs-lisp-mode))
      (prin1 result buf)
      (pp-buffer)
      (let ((lines (count-lines (point-min) (point-max))))
        (if (> lines 1)
            (save-selected-window
              (pop-to-buffer buf)
              (with-current-buffer buf
                (goto-char (point-min))))
          (message "%s" (buffer-substring (point-min) (point-max)))
          (kill-buffer buf))))))

;;;###autoload
(defun +emacs-lisp/repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*ielm*")
       (progn (ielm)
              (let ((buf (get-buffer "*ielm*")))
                (bury-buffer buf)
                buf)))))
